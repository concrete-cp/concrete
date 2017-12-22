package concrete
package generator

import java.security.InvalidParameterException

import com.typesafe.scalalogging.LazyLogging
import concrete.constraint.ReifiedConstraint
import concrete.constraint.linear.{SumEQ, SumLE}
import cspom._
import cspom.util.Finite
import cspom.variable._
import org.scalameter.Quantity

import scala.util.Try

final class ProblemGenerator(val pm: ParameterManager = new ParameterManager()) extends LazyLogging {

  //val generateLargeDomains = pm.getOrElse("generator.generateLargeDomains", false)

  val gm = new GeneratorManager(this)

  @Statistic
  var genTime: Quantity[Double] = _

  def generate(cspom: CSPOM): Try[(Problem, Map[CSPOMVariable[_], Variable])] = {
    val (result, time) = StatisticsManager.measure {

      val variables = generateVariables(cspom)

      val problem = new Problem(variables.values.toArray.sortBy(_.name), goal(cspom.goal, variables))

      val constraints = cspom.constraints.flatMap {
        c => gm.generate(c, variables, cspom).get
      }

      problem.addConstraints(constraints.toSeq)

      logger.info(problem.toString(problem.initState.toState).split("\n").map(l => s"% $l").mkString("\n"))

      logger.info(problem.constraints
        .groupBy {
          case c: ReifiedConstraint => (classOf[ReifiedConstraint], c.positiveConstraint.getClass)
          case c => c.getClass
        }
        .map { case (k, v) => s"$k: ${v.length}" }.mkString("\n"))

      (problem, variables)
    }

    genTime = time
    result
  }

  def isBoolean(constraint: CSPOMConstraint[_]): Boolean = {
    //    if (constraint.function == 'sum) {
    //      val (vars, varParams, constant, mode) = SumGenerator.readCSPOM(constraint)
    //      println(vars)
    //    }

    constraint.nonReified && (constraint.function == 'pseudoboolean || constraint.function == 'sum && {
      val (vars, varParams, constant, mode) = SumGenerator.readCSPOM(constraint)
      (mode == SumLE || mode == SumEQ) &&
        vars.forall {
          case BoolExpression(_) => true
          case e if BoolExpression.is01(e) => true
          case _ => false
        }
    })
  }

  def generateVariables(cspom: CSPOM): Map[CSPOMVariable[_], Variable] = {

    cspom.referencedExpressions.flatMap(_.flatten).collect {
      case v: CSPOMVariable[_] =>
        val dn = cspom.displayName(v)
        require(v.fullyDefined, s"$dn has no bounds. Involved by ${cspom.deepConstraints(v)}")
        require(v.searchSpace > 0, s"$dn has empty domain. Involved by ${cspom.deepConstraints(v)}")
        if (!cspom.isReferenced(v)) logger.warn(s"$dn ($v) is not referenced by constraints")
        v -> new Variable(dn, generateDomain(v))
    }.toMap
  }

  def generateDomain[T](cspomVar: CSPOMVariable[_]): Domain = {

    val dom = cspomVar match {
      case _: BoolVariable => concrete.BooleanDomain()

      case v: IntVariable =>
        if (v.isConvex) {
          val Finite(l) = v.domain.lowerBound
          val Finite(u) = v.domain.upperBound

          (l,u) match {
            case (0, 0) => concrete.BooleanDomain(false)
            case (1, 1) => concrete.BooleanDomain(true)
            case (0, 1) => concrete.BooleanDomain()
            case (lb, ub) => IntDomain.ofInterval(lb, ub)
          }
        } else {
          IntDomain(v.domain)
        }

      case _ => throw new IllegalArgumentException("Unhandled variable type")
    }

    require(cspomVar.searchSpace == dom.size, s"$cspomVar -> $dom")

    dom
  }

  private def goal(goal: Option[WithParam[CSPOMGoal[_]]], variables:Map[CSPOMVariable[_], Variable]): Goal = {

    def obtain(expr: CSPOMVariable[_]): Variable =
      variables.getOrElse(expr, throw new IllegalArgumentException(s"Could not find variable $expr"))

    goal
      .map(_.obj)
      .map {
        case CSPOMGoal.Satisfy => Satisfy
        case CSPOMGoal.Maximize(expr: CSPOMVariable[_]) => Maximize(obtain(expr))
        case CSPOMGoal.Minimize(expr: CSPOMVariable[_]) => Minimize(obtain(expr))
        case CSPOMGoal.Maximize(_: CSPOMConstant[_]) => Satisfy
        case CSPOMGoal.Minimize(_: CSPOMConstant[_]) => Satisfy
        case g => throw new InvalidParameterException("Cannot execute goal " + g)
      }
      .getOrElse(Satisfy)
  }
}
