package concrete.generator.cspompatterns

import cspom.CSPOM
import cspom.CSPOMConstraint
import cspom.compiler.ConstraintCompiler
import cspom.variable.CSPOMConstant
import cspom.variable.CSPOMSeq

/** Removes variables with coef 0 from sum constraints */
object SumFactors extends ConstraintCompiler {

  type A = Seq[_]

  override def mtch(c: CSPOMConstraint[_], p: CSPOM) = {
    if (c.function == 'sum) {
      c.params.get("coefficients").collect {
        case c: Seq[_] if c.contains(0) => c
      }
    } else {
      None
    }
  }

  def compile(c: CSPOMConstraint[_], p: CSPOM, params: A) = {
    val Seq(CSPOMSeq(args), result) = c.arguments

    val (fargs, fparams) = args.zip(params).filter(_._2 != 0).unzip

    replaceCtr(c,
      new CSPOMConstraint(c.result, 'sum,
        Seq(CSPOMSeq(fargs: _*), result), c.params.updated("coefficients", fparams)),
      p)
  }

  def selfPropagation = false

}