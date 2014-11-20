package concrete.generator;

import scala.annotation.tailrec
import scala.reflect.runtime.universe

import com.typesafe.scalalogging.LazyLogging

import concrete.Domain
import concrete.IntDomain
import concrete.ParameterManager
import concrete.Problem
import concrete.Variable
import concrete.generator.constraint.GeneratorManager
import concrete.util.Interval
import cspom.CSPOM
import cspom.CSPOMConstraint
import cspom.Statistic
import cspom.StatisticsManager
import cspom.TimedException
import cspom.VariableNames
import cspom.variable.BoolVariable
import cspom.variable.CSPOMExpression
import cspom.variable.CSPOMVariable
import cspom.variable.FreeVariable
import cspom.variable.IntVariable

final class ProblemGenerator(private val pm: ParameterManager = new ParameterManager()) extends LazyLogging {

  val intToBool = pm.getOrElse("generator.intToBool", false)
  val generateLargeDomains = pm.getOrElse("generator.generateLargeDomains", false)

  private val gm = new GeneratorManager(pm)

  @Statistic
  var genTime: Double = 0.0

  @throws(classOf[FailedGenerationException])
  def generate(cspom: CSPOM): (Problem, Map[CSPOMVariable[_], Variable]) = {
    val (result, time) = try StatisticsManager.time {

      // new ProblemCompiler(cspom).compile();

      //val problem = new Problem();

      val variables = generateVariables(cspom)

      val problem = new Problem(variables.values.toList.sortBy(_.name))

      //      val sorted = cspom.constraints.toSeq.sortBy {
      //        c => (c.fullScope, c.function)
      //      }

      for (
        cspomC <- cspom.constraints;
        constraint <- gm.generate(cspomC, variables)
      ) {
        problem.addConstraint(constraint)
      }

      (problem, variables)
    } catch {
      case t: TimedException =>
        genTime += t.time
        throw t.getCause()
    }
    genTime += time
    result
  }

  def generateVariables(cspom: CSPOM): Map[CSPOMVariable[_], Variable] = {
    val vn = new VariableNames(cspom)

    cspom.referencedExpressions.flatMap(_.flatten).collect {
      case v: CSPOMVariable[_] =>
        require(v.fullyDefined, s"$v has no bounds. Involved by ${cspom.constraints(v)}")
        v -> new Variable(vn.names(v), generateDomain(v))
    }.toMap
  }

  def generateDomain[T](cspomVar: CSPOMVariable[_]): Domain = {

    cspomVar match {
      case bD: BoolVariable => concrete.BooleanDomain()

      case v: IntVariable => IntVariable.iterable(v).toStream match {
        case Seq(0) if intToBool    => concrete.BooleanDomain(false)
        case Seq(1) if intToBool    => concrete.BooleanDomain(true)
        case Seq(0, 1) if intToBool => concrete.BooleanDomain()
        case s =>
          if (v.isConvex) {
            IntDomain.ofInterval(v.head, v.last)
          } else {
            IntDomain(v.asSortedSet)
          }
      }

      case _ => throw new IllegalArgumentException("Unhandled variable type")
    }
  }
}
