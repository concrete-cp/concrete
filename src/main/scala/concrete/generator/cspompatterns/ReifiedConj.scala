package concrete.generator.cspompatterns

import concrete.CSPOMDriver
import cspom.CSPOM
import cspom.CSPOMConstraint
import cspom.compiler.ConstraintCompiler
import cspom.compiler.ConstraintCompilerNoData
import cspom.compiler.Delta
import cspom.variable.BoolExpression
import cspom.variable.BoolVariable
import cspom.variable.CSPOMExpression
import cspom.variable.CSPOMSeq
import cspom.variable.CSPOMVariable
import cspom.variable.SimpleExpression

/**
 * Conjunction is converted to CNF :
 *
 * a = b ^ c ^ d...
 *
 * <=>
 *
 * (a v -b v -c v -d...) ^ (-a v b) ^ (-a v c) ^ (-a v d) ^ ...
 */
object ReifiedConj extends ConstraintCompiler {

  type A = (SimpleExpression[Boolean], Seq[SimpleExpression[Boolean]])

  override def mtch(c: CSPOMConstraint[_], p: CSPOM) = PartialFunction.condOpt(c) {
    case CSPOMConstraint(BoolExpression(res), 'and, a, _) =>
      CSPOMSeq.collectAll(a) {
        case BoolExpression(e) => e
      }
        .map((res, _))
  }
    .flatten

  def compile(fc: CSPOMConstraint[_], problem: CSPOM, data: A) = {
    val (res, args) = data
    val c1 = CSPOMDriver.clause(res)(args: _*)

    val c2 = args.map(v => CSPOMDriver.clause(v)(res))

    replaceCtr(fc, c1 +: c2, problem)

  }

  def selfPropagation = false

}
