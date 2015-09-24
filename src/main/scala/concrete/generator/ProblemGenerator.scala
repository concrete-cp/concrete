package concrete.generator;

import scala.reflect.runtime.universe
import scala.util.Try
import org.scalameter.Quantity
import com.typesafe.scalalogging.LazyLogging
import concrete.Domain
import concrete.IntDomain
import concrete.ParameterManager
import concrete.Problem
import concrete.Variable
import concrete.constraint.linear.Simplex
import concrete.constraint.linear.SumMode
import concrete.constraint.semantic.Clause
import concrete.constraint.semantic.ClauseConstraint
import concrete.constraint.semantic.PseudoBoolean
import concrete.constraint.semantic.SAT
import concrete.generator.constraint.Generator
import concrete.generator.constraint.GeneratorManager
import concrete.generator.constraint.SumGenerator
import cspom.CSPOM
import cspom.Statistic
import cspom.StatisticsManager
import cspom.VariableNames
import cspom.util.Finite
import cspom.variable.BoolVariable
import cspom.variable.CSPOMConstant
import cspom.variable.CSPOMSeq
import cspom.variable.CSPOMVariable
import cspom.variable.IntVariable
import scala.collection.mutable.HashMap

case class LinearConstraint(
    vars: Seq[Variable], factors: Seq[Int],
    mode: SumMode, constant: Int) extends Arc {

  def size = vars.size

}

object Arc {

  def components[T <: Arc](arcs: Seq[T]): Seq[(Set[Variable], Seq[T])] = {
    var edges = Map[Variable, Set[T]]().withDefaultValue(Set())

    for (a <- arcs; v <- a.vars) {
      edges += v -> (edges(v) + a)
    }

    val neighb = edges.mapValues { e => e.map(_.varSet).flatten }

    var visited: Set[Variable] = Set()
    var components: Seq[Set[Variable]] = Seq()

    for (v <- neighb.keys) {
      val component = crawlVariables(v, neighb, visited)
      if (component.nonEmpty) {
        components +:= component
        visited ++= component
      }
    }

    for (vars <- components) yield {
      (vars, vars.flatMap(edges).toSeq)
    }
  }

  def crawlVariables(root: Variable, neighbours: Map[Variable, Set[Variable]], visited: Set[Variable]): Set[Variable] = {
    if (visited(root)) {
      Set()
    } else {
      neighbours(root).foldLeft(visited + root) {
        (visit, n) => visit ++ crawlVariables(n, neighbours, visit)
      }

    }
  }
}

trait Arc {
  def vars: Seq[Variable]
  lazy val varSet = vars.toSet
}

final class ProblemGenerator(private val pm: ParameterManager = new ParameterManager()) extends LazyLogging {

  val intToBool = pm.getOrElse("generator.intToBool", false)
  val generateLargeDomains = pm.getOrElse("generator.generateLargeDomains", false)

  private val gm = new GeneratorManager(pm)

  @Statistic
  var genTime: Quantity[Double] = _

  def generate(cspom: CSPOM): Try[(Problem, Map[CSPOMVariable[_], Variable])] = {
    val (result, time) = StatisticsManager.measure {

      val variables = generateVariables(cspom)

      val problem = new Problem(variables.values.toList.sortBy(_.name))

      val vn = new VariableNames(cspom)

      var clauses: Seq[Clause] = Seq()
      var pb: Seq[PseudoBoolean] = Seq()
      var lin: Seq[LinearConstraint] = Seq()

      cspom.constraints.foreach {
        case c if c.function == 'clause =>
          val Seq(pos: CSPOMSeq[_], neg: CSPOMSeq[_]) = c.arguments
          if (pos.contains(CSPOMConstant(true)) || neg.contains(CSPOMConstant(false))) {
            // Useless clause
          } else {
            val posConc = pos.collect { case v: BoolVariable => Generator.cspom2concreteVar(v)(variables) }.toArray
            val negConc = neg.collect { case v: BoolVariable => Generator.cspom2concreteVar(v)(variables) }.toArray
            clauses +:= Clause(posConc, negConc)
          }
        case constraint if constraint.function == 'pseudoboolean && constraint.nonReified =>

          val (vars, varParams, constant, mode) = SumGenerator.readCSPOM(constraint)

          if (vars.nonEmpty) {
            val solverVariables = vars.map(Generator.cspom2concreteVar(_)(variables))
            pb +:= new PseudoBoolean(solverVariables, varParams, mode, constant)
          }

        case constraint if constraint.function == 'sum && constraint.nonReified =>

          val (vars, varParams, constant, mode) = SumGenerator.readCSPOM(constraint)

          if (vars.nonEmpty) {
            val solverVariables = vars.map(Generator.cspom2concreteVar(_)(variables))
            lin +:= new LinearConstraint(solverVariables, varParams, mode, constant)
          }

        case c =>
          val constraints = gm.generate(c, variables, vn).get
          logger.debug(s"Generating $constraints from $c")
          for (
            constraint <- constraints
          ) {
            problem.addConstraint(constraint)
          }
      }

      if (pm.contains("sat4clauses")) {
        if (pm.contains("sat4pb")) {
          SAT(clauses, pb).foreach(problem.addConstraint)
        } else {
          lin ++:= pb
          SAT(clauses, Seq()).foreach(problem.addConstraint)
        }
      } else {
        clauses.map(new ClauseConstraint(_)).foreach(problem.addConstraint(_))
        if (pm.contains("sat4pb")) {
          SAT(Seq(), pb).foreach(problem.addConstraint)
        } else {
          lin ++:= pb
        }
      }

      if (pm.contains("simplex")) {
        Simplex(lin, pm).foreach(problem.addConstraint(_))
      } else {
        val sg = new SumGenerator(pm)
        for (l <- lin) {
          sg.general(l.vars, l.factors, l.constant, l.mode).toSeq.foreach(problem.addConstraint(_))
        }
      }

      logger.info(problem.toString(problem.initState.toState).split("\n").map(l => s"% $l").mkString("\n"))

      (problem, variables)
    }

    genTime = time
    result
  }

  def generateVariables(cspom: CSPOM): Map[CSPOMVariable[_], Variable] = {
    val vn = new VariableNames(cspom)

    cspom.referencedExpressions.flatMap(_.flatten).collect {
      case v: CSPOMVariable[_] =>
        require(v.fullyDefined, s"${vn.names(v)} has no bounds. Involved by ${cspom.deepConstraints(v)}")
        assert(cspom.isReferenced(v), s"${vn.names(v)} ($v) is not referenced by constraints $cspom}")
        v -> new Variable(vn.names(v), generateDomain(v))
    }.toMap
  }

  def generateDomain[T](cspomVar: CSPOMVariable[_]): Domain = {

    cspomVar match {
      case bD: BoolVariable => concrete.BooleanDomain()

      case v: IntVariable =>
        if (v.isConvex) {
          (v.domain.lowerBound, v.domain.upperBound) match {
            case (Finite(0), Finite(0)) if intToBool => concrete.BooleanDomain(false)
            case (Finite(1), Finite(1)) if intToBool => concrete.BooleanDomain(true)
            case (Finite(0), Finite(1)) if intToBool => concrete.BooleanDomain()
            case (Finite(lb), Finite(ub))            => IntDomain.ofInterval(lb, ub)
          }
        } else {
          IntDomain(v.asSortedSet)
        }

      case _ => throw new IllegalArgumentException("Unhandled variable type")
    }
  }
}
