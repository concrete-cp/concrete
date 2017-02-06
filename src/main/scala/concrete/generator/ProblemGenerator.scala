package concrete
package generator;

import scala.util.Try

import org.scalameter.Quantity

import com.typesafe.scalalogging.LazyLogging

import concrete.constraint.ReifiedConstraint
import concrete.constraint.semantic.Clause
import concrete.constraint.semantic.PseudoBoolean
import concrete.constraint.semantic.SAT
import cspom.CSPOM
import cspom.Statistic
import cspom.StatisticsManager
import cspom.util.Finite
import cspom.variable.BoolVariable
import cspom.variable.CSPOMSeq
import cspom.variable.CSPOMVariable
import cspom.variable.IntVariable
import cspom.CSPOMConstraint
import cspom.variable.BoolExpression
import concrete.constraint.linear.SumEQ
import concrete.constraint.linear.SumLE

final class ProblemGenerator(val pm: ParameterManager = new ParameterManager()) extends LazyLogging {

  val intToBool = pm.getOrElse("generator.intToBool", true)
  //val generateLargeDomains = pm.getOrElse("generator.generateLargeDomains", false)

  val gm = new GeneratorManager(this)

  @Statistic
  var genTime: Quantity[Double] = _

  def isBoolean(constraint: CSPOMConstraint[_]) = {
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

  def generate(cspom: CSPOM): Try[(Problem, Map[CSPOMVariable[_], Variable])] = {
    val (result, time) = StatisticsManager.measure {

      val variables = generateVariables(cspom)

      val problem = new Problem(variables.values.toArray.sortBy(_.name))

      var clauses: Seq[Clause] = Seq.empty
      var pb: Seq[PseudoBoolean] = Seq.empty

      val constraints = cspom.constraints.flatMap {
        case c if c.function == 'clause && pm.contains("sat4clauses") =>
          val Seq(pos: CSPOMSeq[_], neg: CSPOMSeq[_]) = c.arguments
          if (!pos.exists(_.isTrue) && !neg.exists(_.isFalse)) {
            val posConc = pos.map { v => Generator.cspom2concreteVar(v)(variables) }.toArray
            val negConc = neg.map { v => Generator.cspom2concreteVar(v)(variables) }.toArray
            clauses +:= Clause(posConc, negConc)
          }
          Seq()

        case constraint if isBoolean(constraint) && constraint.nonReified && pm.contains("sat4pb") =>
          val (vars, varParams, constant, mode) = SumGenerator.readCSPOM(constraint)

          if (vars.nonEmpty) {
            val solverVariables = vars.map(Generator.cspom2concreteVar(_)(variables))
            pb +:= new PseudoBoolean(solverVariables, varParams, mode, constant)
          }
          Seq()

        case c =>
          gm.generate(c, variables, cspom).get
      }

      problem.addConstraints(constraints.toSeq)
      problem.addConstraints(SAT(clauses, pb, pm))

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
      case bD: BoolVariable => concrete.BooleanDomain()

      case v: IntVariable =>
        if (v.isConvex) {
          (v.domain.lowerBound, v.domain.upperBound) match {
            case (Finite(0), Finite(0)) if intToBool => concrete.BooleanDomain(false)
            case (Finite(1), Finite(1)) if intToBool => concrete.BooleanDomain(true)
            case (Finite(0), Finite(1)) if intToBool => concrete.BooleanDomain()
            case (Finite(lb), Finite(ub)) => IntDomain.ofInterval(lb, ub)
          }
        } else {
          IntDomain(v.domain)
        }

      case _ => throw new IllegalArgumentException("Unhandled variable type")
    }

    require(cspomVar.searchSpace == dom.size, s"$cspomVar -> $dom")

    dom
  }
}
