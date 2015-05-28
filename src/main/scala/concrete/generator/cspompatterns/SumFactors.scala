package concrete.generator.cspompatterns

import cspom.CSPOM
import cspom.CSPOMConstraint
import cspom.compiler.ConstraintCompiler
import cspom.variable.CSPOMConstant
import cspom.variable.CSPOMSeq
import cspom.variable.CSPOMExpression

/** Removes variables with coef 0 from sum constraints */
object SumFactors extends ConstraintCompiler {

  type A = (Seq[CSPOMExpression[_]], Seq[Int], Int)

  override def mtch(c: CSPOMConstraint[_], p: CSPOM) = {
    if (c.function == 'sum) {
      c.getParam[Seq[Int]]("coefficients")
        .flatMap { params =>
          val Seq(CSPOMSeq(args), CSPOMConstant(result: Int)) = c.arguments
          val (fargs, fparams) = args.zip(params).filter(_._2 != 0).unzip
          val gcd = this.gcd(fparams).toInt
          val rparams = fparams.map(_ / gcd)
          val rresult = result / gcd
          if (rparams != params || rresult != result) {
            Some(fargs, rparams, rresult)
          } else {
            None
          }
        }
    } else {
      None
    }
  }

  def compile(c: CSPOMConstraint[_], p: CSPOM, data: A) = {
    val (args, params, result) = data

    replaceCtr(c,
      new CSPOMConstraint(c.result, 'sum,
        Seq(CSPOMSeq(args: _*), CSPOMConstant(result)), c.params.updated("coefficients", params)),
      p)
  }

  def selfPropagation = false

  /** Make sure not to divide by some negative number as it would reverse the inequality **/
  def gcd(a: Seq[Int]): BigInt = a.map(BigInt(_)).reduce(_.gcd(_)).abs

  def gcd(ia: Int, ib: Int): Int = {
    var d = 0
    var a = ia
    var b = ib
    while (even(a) && even(b)) {
      a /= 2
      b /= 2
      d += 1
    }
    while (a != b) {
      if (even(a)) {
        a /= 2
      } else if (even(b)) {
        b /= 2
      } else if (a > b) {
        a = (a - b) / 2
      } else {
        b = (b - a) / 2
      }
    }

    a * (0x1 << d)
  }

  def even(a: Int) = (a & 0x1) == 0
}