package concrete
package generator

import java.security.InvalidParameterException

import com.typesafe.scalalogging.LazyLogging
import concrete.constraint.linear.SumMode._
import concrete.constraint.{Constraint, ReifiedConstraint}
import cspom._
import cspom.variable._

import scala.util.{Failure, Success, Try}

final class ProblemGenerator(val pm: ParameterManager = new ParameterManager()) extends LazyLogging {

  //val generateLargeDomains = pm.getOrElse("generator.generateLargeDomains", false)

  val gm = new GeneratorManager(this)

  @Statistic
  var genTime: Double = _

  def generate(cspom: CSPOM): Try[(Problem, Map[CSPOMVariable[_], Variable])] = {
    val (result, time) = StatisticsManager.measureTry {

      for {
        variables <- generateVariables(cspom)
        constraints <- generateConstraints(variables, cspom)
      } yield {
        val problem = new Problem(variables.values.toArray.sortBy(_.name), goal(cspom.goal, variables))
          .addConstraints(constraints)
        logger.info(problem.toString(problem.initState.toState).split("\n").map(l => s"% $l").mkString("\n"))

        logger.info(problem.constraints
          .groupBy {
            case c: ReifiedConstraint => (classOf[ReifiedConstraint], c.constraint.getClass)
            case c => c.getClass
          }
          .map { case (k, v) => s"$k: ${v.size}" }.mkString("\n"))

        (problem, variables)
      }
    }

    genTime = time
    result
  }

  def generateConstraints(variables: Map[CSPOMVariable[_], Variable], cspom: CSPOM): Try[Seq[Constraint]] = {
    val (failures, successes) =
      cspom.constraints.map(c => gm.generate(c, variables, cspom)).toSeq
        .partitionMap(_.toEither)

    failures.headOption.map(Failure(_)).getOrElse(Success(successes.flatten))

  }

  def generateVariables(cspom: CSPOM): Try[Map[CSPOMVariable[_], Variable]] = {

    val (failures, successes) = cspom.referencedExpressions.flatMap(_.flatten).collect {
      case v: CSPOMVariable[_] =>
        lazy val dn = cspom.displayName(v)
        require(v.fullyDefined, s"$dn has no bounds. Involved by ${cspom.deepConstraints(v).map(_.toString(cspom.displayName)).mkString(", ")}")
        require(v.searchSpace > 0, s"$dn has empty domain. Involved by ${cspom.deepConstraints(v)}")
        if (!cspom.isReferenced(v)) logger.warn(s"$dn ($v) is not referenced by constraints")
        generateDomain(v)
          .recoverWith {
            case e =>
              Failure(new CSPParseException(
                s"Failed to generate $dn, involved by ${cspom.deepConstraints(v)}",
                e))
          }
          .map(gn => v -> new Variable(dn, gn))
    }
      .partitionMap(_.toEither)

    failures.headOption.map(Failure(_)).getOrElse(Success(successes.toMap))

  }

  def generateDomain[T](cspomVar: CSPOMVariable[_]): Try[Domain] = {

    val dom = cspomVar match {
      case _: BoolVariable =>
        Success(concrete.BooleanDomain())

      case v: IntVariable =>
        if (v.isConvex) {
          val l = cspom.util.Math.toIntExact(v.domain.lowerBound.finite)
          val u = cspom.util.Math.toIntExact(v.domain.upperBound.finite)

          (l, u) match {
            case (0, 0) =>
              Success(concrete.BooleanDomain(false))
            case (1, 1) =>
              Success(concrete.BooleanDomain(true))
            case (0, 1) =>
              Success(concrete.BooleanDomain())
            case (lb, ub) =>
              Success(IntDomain.ofInterval(lb, ub))
          }
        } else {
          Success(IntDomain(v.domain))
        }

      case _ => Failure(new IllegalArgumentException("Unhandled variable type"))
    }

    for (d <- dom) {
      require(cspomVar.searchSpace == d.size, s"$cspomVar -> $dom")
    }

    dom
  }

  private def goal(goal: Option[WithParam[CSPOMGoal[_]]], variables: Map[CSPOMVariable[_], Variable]): Goal = {

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

  def isBoolean(constraint: CSPOMConstraint[_]): Boolean = {
    //    if (constraint.function == "sum") {
    //      val (vars, varParams, constant, mode) = SumGenerator.readCSPOM(constraint)
    //      println(vars)
    //    }

    constraint.nonReified && (constraint.function == "pseudoboolean" || constraint.function == "sum" && {
      val (vars, _, _, mode) = SumGenerator.readCSPOM(constraint)
      (mode == LE || mode == EQ) &&
        vars.forall {
          case BoolExpression(_) => true
          case e if BoolExpression.is01(e) => true
          case _ => false
        }
    })
  }
}
