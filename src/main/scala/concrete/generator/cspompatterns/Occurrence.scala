package concrete.generator.cspompatterns

import cspom.CSPOM
import cspom.CSPOMConstraint
import cspom.compiler.ConstraintCompilerNoData
import cspom.compiler.Delta

/**
 * @author vion
 */
object Occurrence extends ConstraintCompilerNoData {

  def matchBool(c: CSPOMConstraint[_], p: CSPOM) = c.function == 'occurrence

  def compile(c: CSPOMConstraint[_], p: CSPOM): Delta = {
    val CSPOMConstraint(r, _, Seq(v, args), params) = c

    val atMost = CSPOMConstraint('atMost)(r, v, args) withParams params
    val atLeast = CSPOMConstraint('atLeast)(r, v, args) withParams params

    replaceCtr(c, Seq(atMost, atLeast), p)

  }
}