package concrete.generator.cspompatterns

import cspom.compiler.ConstraintCompiler
import cspom.compiler.ConstraintCompilerNoData
import cspom.CSPOM
import cspom.CSPOMConstraint
import cspom.variable.CSPOMConstant
import concrete.constraint.semantic.SumMode
import concrete.constraint.semantic.SumNE
import cspom.variable.CSPOMSeq
import cspom.variable.CSPOMVariable
import SumMode._
import cspom.variable.CSPOMExpression
import concrete.CSPOMDriver
import cspom.variable.IntExpression

/**
 *  Merge duplicates in linear constraints (x + x = 0 -> 2.x = 0). Also remove variables with factor = 0.
 */
object SumDuplicates extends ConstraintCompiler {

  type A = (collection.Map[CSPOMExpression[_], Int], Int)

  override def mtch(c: CSPOMConstraint[_], p: CSPOM) = PartialFunction.condOpt(c) {
    case CSPOMConstraint(CSPOMConstant(true), 'sum, Seq(CSPOMSeq(vars), CSPOMConstant(const: Int)), p) =>

      val params = p.get("coefficients")
        .map(_.asInstanceOf[Seq[Int]])
        .getOrElse(Seq.fill(vars.length)(1))

      var duplicates = false
      var factors = collection.mutable.Map[CSPOMExpression[_], Int]()
      for ((v, c) <- (vars, params).zipped) {
        factors.get(v) match {
          case Some(i) =>
            duplicates = true
            factors(v) = i + c
          case None =>
            duplicates |= (c == 0)
            factors(v) = c
        }
      }

      if (duplicates) {
        Some((factors, const))
      } else {
        None
      }

  }
    .flatten

  def compile(constraint: CSPOMConstraint[_], p: CSPOM, data: A) = {
    val (args, const) = data
    val (IntExpression.seq(variables), factors) = args.filter(_._2 != 0).unzip

    val newConstraint =
      CSPOMDriver.linear(variables, factors.toSeq, constraint.getParam[String]("mode").get, const)

    //      CSPOMConstraint(
    //      'sum,
    //      Seq(CSPOMSeq(variables.toSeq: _*), CSPOMConstant(const)),
    //      constraint.params + ("coefficients" -> factors.toSeq))

    //println(s"replacing $constraint with $newConstraint")
    replaceCtr(constraint, newConstraint, p)

  }

  def selfPropagation = false

}