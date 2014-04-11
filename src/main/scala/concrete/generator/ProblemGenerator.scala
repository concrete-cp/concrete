package concrete.generator;

import concrete.generator.constraint.GeneratorManager
import concrete.{ Domain, IntDomain, Problem }
import cspom.CSPOMConstraint
import cspom.CSPOM
import scala.annotation.tailrec
import scala.collection.immutable.Queue
import cspom.variable.IntInterval
import com.typesafe.scalalogging.slf4j.LazyLogging
import concrete.UndefinedDomain
import concrete.Variable
import cspom.variable.CSPOMVariable
import cspom.variable.BoolVariable
import cspom.variable.IntVariable
import cspom.variable.FreeVariable
import cspom.variable.IntSeq
import cspom.variable.FreeInt
import cspom.variable.CSPOMSeq
import cspom.variable.CSPOMExpression
import cspom.variable.CSPOMConstant
import cspom.compiler.QueueSet
import concrete.Parameter
import cspom.Statistic
import cspom.StatisticsManager
import cspom.TimedException
import cspom.VariableNames
import concrete.generator.constraint.GeneratorManager
import concrete.ParameterManager

final class ProblemGenerator(private val pm: ParameterManager = new ParameterManager()) extends LazyLogging {

  @Parameter("generator.intToBool")
  val intToBool = false

  @Parameter("generator.generateLargeDomains")
  val generateLargeDomains = false

  pm.register(this)

  private val gm = new GeneratorManager(pm)

  @Statistic
  var genTime: Double = 0.0

  @throws(classOf[FailedGenerationException])
  def generate(cspom: CSPOM): (Problem, Map[CSPOMVariable[_], Variable]) = {
    val (result, time) = try StatisticsManager.time {

      // new ProblemCompiler(cspom).compile();

      //val problem = new Problem();

      val variables = generateVariables(cspom)

      val problem = new Problem(variables.values.toList)

      val failed = fixPoint(cspom.constraints.toList, genConstraint(_: CSPOMConstraint[_], variables, problem))

      val failed2 = if (generateLargeDomains) {
        genLarge(failed, variables, problem)
      } else {
        failed
      }

      require(failed2.isEmpty, "Could not generate constraints " + failed2)

      (problem, variables)
    } catch {
      case t: TimedException =>
        genTime += t.time
        throw t.getCause()
    }
    genTime += time
    result
  }

  def genLarge(failed: Seq[CSPOMConstraint[_]], variables: Map[CSPOMVariable[_], Variable], problem: Problem): Seq[CSPOMConstraint[_]] = {
    for (v <- problem.variables if v.dom.undefined) {
      logger.warn("Generating arbitrary domain for " + v)
      v.dom = IntDomain(-1000000 to 1000000)
    }

    fixPoint(failed, genConstraint(_: CSPOMConstraint[_], variables, problem))
  }

  def genConstraint(c: CSPOMConstraint[_], variables: Map[CSPOMVariable[_], Variable], problem: Problem): Boolean = {
    gm.generate(c, variables, problem) match {
      case Some(s) =>
        s.foreach(problem.addConstraint(_))
        false
      case None => true
    }
  }

  @tailrec
  def fixPoint[A](list: Seq[A], process: A => Boolean): Seq[A] = {
    val l2 = list.filter(process)
    if (list == l2) {
      list
    } else {
      fixPoint(l2, process)
    }
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

  def generateDomain[T](cspomVar: CSPOMVariable[_]): Domain = cspomVar match {
    case bD: BoolVariable =>
      new concrete.BooleanDomain();

    case v: IntVariable => v.domain match {
      case Seq(0) if intToBool => new concrete.BooleanDomain(false)
      case Seq(1) if intToBool => new concrete.BooleanDomain(true)
      case Seq(0, 1) if intToBool => new concrete.BooleanDomain()
      case int: IntInterval => IntDomain(int.range)
      case IntSeq(seq) => IntDomain(seq)
      case FreeInt => UndefinedDomain
    }

    case _: FreeVariable => UndefinedDomain
  }
}
