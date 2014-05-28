package concrete.generator;

import scala.annotation.tailrec
import scala.collection.SortedSet
import scala.reflect.runtime.universe

import com.typesafe.scalalogging.slf4j.LazyLogging

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
import cspom.util.RangeSet
import cspom.variable.BoolVariable
import cspom.variable.CSPOMConstant
import cspom.variable.CSPOMExpression
import cspom.variable.CSPOMSeq
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

      for (
        cspomC <- cspom.constraints;
        concreteC <- gm.generate(cspomC, variables, problem)
      ) {
        problem.addConstraint(concreteC)
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
      case v: CSPOMVariable[_] => v -> new Variable(vn.names(v), generateDomain(v))
    }.toMap

  }

  def generateVariables(name: String, e: CSPOMExpression[_]): Map[CSPOMVariable[_], Variable] =
    e match {
      case c: CSPOMConstant[_] => Map()
      case v: CSPOMVariable[_] => Map(v -> new Variable(name, generateDomain(v)))
      case CSPOMSeq(vars, range, _) =>
        (vars zip range).flatMap {
          case (e, i) => generateVariables(s"$name[$i]", e)
        } toMap
      case _ => throw new UnsupportedOperationException(s"Cannot generate $e")
    }

  def generateDomain[T](cspomVar: CSPOMVariable[_]): Domain = {
    require(cspomVar.fullyDefined)
    cspomVar match {
      case bD: BoolVariable =>
        new concrete.BooleanDomain();

      case v: IntVariable => v.toStream match {
        case Seq(0, 1) if intToBool => new concrete.BooleanDomain(false)
        case Seq(1) if intToBool => new concrete.BooleanDomain(true)
        case Seq(0, 1) if intToBool => new concrete.BooleanDomain()
        case s =>
          if (v.domain.isConvex) {
            IntDomain(Interval(v.head, v.last))
          } else {
            IntDomain(SortedSet[Int]() ++ s)
          }
      }

      case _ => throw new IllegalArgumentException("Unhandled variable type")
    }
  }
}
