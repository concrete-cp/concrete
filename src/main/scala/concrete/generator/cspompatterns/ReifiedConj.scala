package concrete.generator.cspompatterns

import cspom.CSPOM
import cspom.CSPOMConstraint
import cspom.compiler.{ConstraintCompiler, ConstraintCompilerNoData}
import cspom.variable.CSPOMSeq

/**
 * Conjunction is converted to CNF :
 *
 * a = b ^ c ^ d...
 *
 * <=>
 *
 * (a v -b v -c v -d...) ^ (-a v b) ^ (-a v c) ^ (-a v d) ^ ...
 */
object ReifiedConj extends ConstraintCompilerNoData {

  override def matchBool(c: CSPOMConstraint[_], p: CSPOM) = c.function == 'and

  def compile(fc: CSPOMConstraint[_], problem: CSPOM) = {

    val res = fc.result
    val args = fc.arguments
    val c1 = CSPOMConstraint('clause)(CSPOMSeq(res), CSPOMSeq(args: _*))
    val c2 = args.map(v => CSPOMConstraint('clause)(CSPOMSeq(v), CSPOMSeq(res)))
    ConstraintCompiler.replaceCtr(fc, c1 +: c2, problem)
  }

  def selfPropagation = false

}
