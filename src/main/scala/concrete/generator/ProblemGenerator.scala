package concrete.generator;

import scala.annotation.elidable

import scala.reflect.runtime.universe
import scala.util.Try

import org.scalameter.Quantity

import com.typesafe.scalalogging.LazyLogging

import concrete.Domain
import concrete.IntDomain
import concrete.ParameterManager
import concrete.Problem
import concrete.Variable
import concrete.cluster.Arc
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

case class LinearConstraint(
    vars: Seq[Variable], factors: Seq[Int],
    mode: SumMode, constant: Int) extends Arc {

  def size = vars.size

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

      val problem = new Problem(variables.values.toArray.sortBy(_.name))

      val vn = new VariableNames(cspom)

      var clauses: Seq[Clause] = Seq.empty
      var pb: Seq[PseudoBoolean] = Seq.empty
      var lin: Seq[LinearConstraint] = Seq.empty

      val constraints = cspom.constraints.flatMap {
        case c if c.function == 'clause =>
          val Seq(pos: CSPOMSeq[_], neg: CSPOMSeq[_]) = c.arguments
          if (!pos.contains(CSPOMConstant(true)) && !neg.contains(CSPOMConstant(false))) {
            val posConc = pos.collect { case v: BoolVariable => Generator.cspom2concreteVar(v)(variables) }.toArray
            val negConc = neg.collect { case v: BoolVariable => Generator.cspom2concreteVar(v)(variables) }.toArray
            clauses +:= Clause(posConc, negConc)
          }
          Seq()

        case constraint if constraint.function == 'pseudoboolean && constraint.nonReified =>
          val (vars, varParams, constant, mode) = SumGenerator.readCSPOM(constraint)

          if (vars.nonEmpty) {
            val solverVariables = vars.map(Generator.cspom2concreteVar(_)(variables))
            pb +:= new PseudoBoolean(solverVariables, varParams, mode, constant)
          }
          Seq()

        case constraint if constraint.function == 'sum && constraint.nonReified =>
          val (vars, varParams, constant, mode) = SumGenerator.readCSPOM(constraint)

          if (vars.nonEmpty) {
            val solverVariables = vars.map(Generator.cspom2concreteVar(_)(variables))
            lin +:= new LinearConstraint(solverVariables, varParams, mode, constant)
          }
          Seq()

        case c =>
          gm.generate(c, variables, vn).get
      }

      problem.addConstraints(constraints.toSeq)

      if (pm.contains("sat4clauses")) {
        if (pm.contains("sat4pb")) {
          problem.addConstraints(
            SAT(clauses, pb))
        } else {
          lin ++:= pb
          problem.addConstraints(
            SAT(clauses = clauses))
        }
      } else {
        problem.addConstraints(
          clauses.map(new ClauseConstraint(_)))
        if (pm.contains("sat4pb")) {
          problem.addConstraints(
            SAT(pb = pb))
        } else {
          lin ++:= pb
        }
      }

      if (pm.contains("simplex")) {
        problem.addConstraints(
          Simplex(lin, pm))
      } else {
        val sg = new SumGenerator(pm)
        problem.addConstraints(
          lin.flatMap(l =>
            sg.general(l.vars, l.factors, l.constant, l.mode).toSeq))

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
