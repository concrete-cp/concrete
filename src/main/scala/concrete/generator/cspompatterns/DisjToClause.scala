package concrete.generator.cspompatterns

import cspom.CSPOM
import scala.collection.mutable.Queue
import cspom.compiler.ConstraintCompiler
import cspom.CSPOMConstraint
import cspom.variable.CSPOMVariable
import cspom.compiler.Delta
import cspom.variable.BoolVariable
import cspom.variable.CSPOMConstant
import cspom.variable.CSPOMConstant
import cspom.compiler.ConstraintCompilerNoData
import cspom.variable.CSPOMExpression
import cspom.variable.SimpleExpression
import cspom.variable.CSPOMSeq

/**
 * Disjunction to clauses (for XCSP 2)
 */
object DisjToClause extends ConstraintCompilerNoData {

  def matchBool(c: CSPOMConstraint[_], p: CSPOM) = c.function == 'or

  def compile(fc: CSPOMConstraint[_], problem: CSPOM) = {

    replaceCtr(fc, CSPOMConstraint(fc.result, 'clause, Seq(CSPOMSeq(fc.arguments: _*), CSPOMSeq())), problem)

  }

  def selfPropagation = false

}
