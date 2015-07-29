package concrete.generator;

import scala.reflect.runtime.universe
import com.typesafe.scalalogging.LazyLogging
import concrete.Domain
import concrete.IntDomain
import concrete.ParameterManager
import concrete.Problem
import concrete.Variable
import concrete.generator.constraint.GeneratorManager
import cspom.CSPOM
import cspom.Statistic
import cspom.StatisticsManager
import cspom.TimedException
import cspom.VariableNames
import cspom.variable.BoolVariable
import cspom.variable.CSPOMVariable
import cspom.variable.IntVariable
import cspom.variable.CSPOMSeq
import cspom.variable.CSPOMConstant
import concrete.generator.constraint.Generator
import concrete.constraint.semantic.Clause
import concrete.constraint.semantic.SAT
import concrete.constraint.semantic.PseudoBoolean
import concrete.generator.constraint.Var
import concrete.constraint.semantic.SumMode
import concrete.generator.constraint.Const
import concrete.constraint.semantic.SumMode._
import concrete.generator.constraint.SumGenerator
import scala.util.Try
import cspom.util.Finite

final class ProblemGenerator(private val pm: ParameterManager = new ParameterManager()) extends LazyLogging {

  val intToBool = pm.getOrElse("generator.intToBool", false)
  val generateLargeDomains = pm.getOrElse("generator.generateLargeDomains", false)

  private val gm = new GeneratorManager(pm)

  @Statistic
  var genTime: Double = 0.0

  def generate(cspom: CSPOM): Try[(Problem, Map[CSPOMVariable[_], Variable])] = {
    val (result, time) = StatisticsManager.time {

      val variables = generateVariables(cspom)

      val problem = new Problem(variables.values.toList.sortBy(_.name))

      val vn = new VariableNames(cspom)

      var clauses: Seq[Clause] = Seq()
      var pb: Seq[PseudoBoolean] = Seq()

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
        case constraint if constraint.function == 'pseudoboolean =>

          val (vars, varParams, constant, mode) = SumGenerator.readCSPOM(constraint)

          if (vars.nonEmpty) {
            val solverVariables = vars.map(Generator.cspom2concreteVar(_)(variables))
            pb +:= PseudoBoolean(solverVariables, varParams, mode, constant)
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

      SAT(clauses, pb, pm).foreach(problem.addConstraint)

      logger.info(problem.toString(problem.initState.toState).split("\n").map(l => s"% $l").mkString("\n"))

      (problem, variables)
    }

    genTime += time
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
