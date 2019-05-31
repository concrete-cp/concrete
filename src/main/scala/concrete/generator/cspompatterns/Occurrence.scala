package concrete.generator.cspompatterns

import cspom.CSPOM
import cspom.CSPOMConstraint
import cspom.compiler.{ConstraintCompiler, ConstraintCompilerNoData, Delta, Functions}

/**
 * @author vion
 */
object Occurrence extends ConstraintCompilerNoData {

  def functions = Functions('occurrence)

  def matchBool(c: CSPOMConstraint[_], p: CSPOM) = true

  def compile(c: CSPOMConstraint[_], p: CSPOM): Delta = {
    val CSPOMConstraint(r, _, Seq(v, args), params) = c

    val atMost = CSPOMConstraint('atMost)(r, v, args) withParams params
    val atLeast = CSPOMConstraint('atLeast)(r, v, args) withParams params

    ConstraintCompiler.replaceCtr(c, Seq(atMost, atLeast), p)

  }
}