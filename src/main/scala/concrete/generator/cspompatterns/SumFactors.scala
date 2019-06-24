package concrete.generator.cspompatterns

import concrete.generator.SumGenerator
import cspom.CSPOM._
import cspom.{CSPOM, CSPOMConstraint}
import cspom.compiler.{ConstraintCompiler, Delta, Functions}
import cspom.variable.CSPOMExpression

/** Removes variables with coef 0 from sum constraints and simplifies factors */
object SumFactors extends ConstraintCompiler {

  type A = (Seq[CSPOMExpression[Any]], Seq[Int], Int)

  def functions = Functions("sum")

  override def mtch(c: CSPOMConstraint[_], p: CSPOM): Option[(Seq[CSPOMExpression[Any]], Seq[Int], Int)] = {
    val (args, coefs, result, _) = SumGenerator.readCSPOM(c)

    val (fargs, fparams) = args.zip(coefs).filter(_._2 != 0).unzip
    assert(fparams.nonEmpty, c)
    val gcd = concrete.util.Math.toIntExact(concrete.util.Math.gcd(result +: fparams).abs)
    if (gcd == 1 && fargs.lengthCompare(args) == 0) {
      None
    } else {
      val rparams = fparams.map(_ / gcd)
      val rresult = result / gcd

      Some((fargs, rparams, rresult))
    }
  }

  def compile(c: CSPOMConstraint[_], p: CSPOM, data: A): Delta = {
    val (args, coefs, result) = data

    ConstraintCompiler.replaceCtr(c,
      CSPOMConstraint(c.result)("sum")(coefs, args, result) withParams c.params,
      p)
  }

  def selfPropagation = false

}