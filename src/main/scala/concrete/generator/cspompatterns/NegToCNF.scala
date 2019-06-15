package concrete.generator.cspompatterns

import concrete.CSPOMDriver
import cspom.{CSPOM, CSPOMConstraint}
import cspom.compiler.{ConstraintCompiler, ConstraintCompilerNoData, Delta, Functions}
import cspom.variable.CSPOMSeq

/**
  * Negation is converted to CNF :
  *
  * a = -b <=> (a v b) ^ (-a v -b)
  **/
//  private def generateNeg(result: Variable, arg: Variable) {
//
//    addConstraint(new Disjunction(result, arg));
//    addConstraint(new Disjunction(Array(result, arg), Array(true, true)));
//
//  }
object NegToCNF extends ConstraintCompilerNoData {

  def functions = Functions("not")

  override def matchBool(c: CSPOMConstraint[_], p: CSPOM) = true

  def compile(fc: CSPOMConstraint[_], problem: CSPOM): Delta = {

    val res = fc.result
    val Seq(arg) = fc.arguments

    val newConstraints = Seq(
      CSPOMDriver.clause(CSPOMSeq(res, arg), CSPOMSeq()),
      CSPOMDriver.clause(CSPOMSeq(), CSPOMSeq(res, arg)))

    ConstraintCompiler.replaceCtr(fc, newConstraints, problem)

  }

  def selfPropagation = false
}
