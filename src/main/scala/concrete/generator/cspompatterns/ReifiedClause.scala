package concrete.generator.cspompatterns

import concrete.CSPOMDriver
import cspom.{CSPOM, CSPOMConstraint}
import cspom.compiler.{ConstraintCompiler, Delta, Functions}
import cspom.variable.{CSPOMExpression, CSPOMSeq}

/**
  * Reified disjunction is converted to CNF :
  *
  * a = b v c v d...
  *
  * <=>
  *
  * (-a v b v c v d...) ^ (a v -b) ^ (a v -c) ^ (a v -d) ^ ...
  */
object ReifiedClause extends ConstraintCompiler {

  type A = (CSPOMExpression[_], CSPOMSeq[_], Seq[CSPOMExpression[_]])

  def functions: Functions = Functions('clause)

  override def constraintMatcher: PartialFunction[CSPOMConstraint[_], (CSPOMExpression[Any], CSPOMSeq[_], Seq[CSPOMExpression[Any]])] = {
    case CSPOMConstraint(res, _, Seq(p: CSPOMSeq[_], CSPOMSeq(n)), params) if !res.isTrue =>
      (res, p, n)
  }

  def compile(fc: CSPOMConstraint[_], problem: CSPOM, data: A): Delta = {
    val (res, positive, negative) = data

    val c1 = CSPOMDriver.clause(positive, CSPOMSeq(res +: negative: _*))
    val c2 = positive.map {
      v => CSPOMDriver.clause(res)(v)
    }
    val c3 = negative.map {
      v => CSPOMDriver.clause(res, v)()
    }

    ConstraintCompiler.replaceCtr(fc, c1 +: (c2 ++ c3), problem)

  }

  def selfPropagation = false
}
