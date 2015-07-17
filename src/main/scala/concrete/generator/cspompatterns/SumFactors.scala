package concrete.generator.cspompatterns

import cspom.CSPOM
import cspom.CSPOMConstraint
import cspom.compiler.ConstraintCompiler
import cspom.variable.CSPOMConstant
import cspom.variable.CSPOMExpression
import cspom.variable.CSPOMSeq
import cspom.variable.IntExpression

/** Removes variables with coef 0 from sum constraints and simplifies factors */
object SumFactors extends ConstraintCompiler {

  type A = (Seq[CSPOMExpression[_]], Seq[Int], Int)

  override def mtch(c: CSPOMConstraint[_], p: CSPOM) = {
    c match {
      case CSPOMConstraint(_, 'sum, Seq(IntExpression.constSeq(coefs), CSPOMSeq(args), CSPOMConstant(result: Int)), _) =>

        val (fargs, fparams) = args.zip(coefs).filter(_._2 != 0).unzip
        require(fparams.nonEmpty, c)
        val gcd = concrete.util.Math.gcd(fparams).abs.toInt
        val rparams = fparams.map(_ / gcd)
        val rresult = result / gcd
        if (rparams != coefs || rresult != result) {
          Some(fargs, rparams, rresult)
        } else {
          None
        }
      case _ =>
        None
    }
  }

  def compile(c: CSPOMConstraint[_], p: CSPOM, data: A) = {
    val (args, coefs, result) = data

    replaceCtr(c,
      CSPOMConstraint(c.result)('sum)(CSPOMConstant.ofSeq(coefs), CSPOMSeq(args: _*), CSPOMConstant(result)) withParams c.params,
      p)
  }

  def selfPropagation = false

}