package concrete.generator.cspompatterns

import cspom.CSPOMConstraint
import cspom.compiler.GlobalCompiler
import cspom.variable.CSPOMConstant
import cspom.compiler.Ctr
import cspom.variable.CSPOMSeq
import cspom.compiler.ConstraintCompiler
import cspom.compiler.ConstraintCompilerNoData
import cspom.CSPOM
import cspom.variable.BoolExpression
import cspom.variable.BoolVariable

object XCSPPatterns {
  def apply() = Seq(
    new GlobalCompiler(mtch) { def selfPropagation = true },
    new ConstraintCompilerNoData {
      def matchBool(c: CSPOMConstraint[_], problem: CSPOM) = c.function == 'ne
      def compile(constraint: CSPOMConstraint[_], problem: CSPOM) = {
        val CSPOMConstraint(a, _, args, params) = constraint

        val n = new BoolVariable()
        val c1 = CSPOMConstraint(n, 'not, Seq(a))
        val c2 = CSPOMConstraint(n, 'eq, args, params)

        replaceCtr(Seq(constraint), Seq(c1, c2), problem)

      }

      def selfPropagation: Boolean = false
    })

  val mtch: PartialFunction[CSPOMConstraint[_], CSPOMConstraint[_]] = {
    case CSPOMConstraint(a, 'sub, Seq(b, c), p) =>
      CSPOMConstraint(
        'sum,
        Seq(CSPOMSeq(a, b, c), CSPOMConstant(0)),
        p + ("coefficients" -> Seq(-1, 1, -1)) + ("mode" -> "eq"))

    case CSPOMConstraint(a, 'add, Seq(b, c), p) =>
      CSPOMConstraint(
        'sum,
        Seq(CSPOMSeq(a, b, c), CSPOMConstant(0)),
        p + ("coefficients" -> Seq(-1, 1, 1)) + ("mode" -> "eq"))

    case CSPOMConstraint(r, 'lt, Seq(a, b), p) =>
      CSPOMConstraint(r, 'sum, Seq(CSPOMSeq(a, b), CSPOMConstant(0)),
        p + ("coefficients" -> Seq(1, -1)) + ("mode" -> "lt"))

    case CSPOMConstraint(r, 'le, Seq(a, b), p) =>
      CSPOMConstraint(r, 'sum, Seq(CSPOMSeq(a, b), CSPOMConstant(0)),
        p + ("coefficients" -> Seq(1, -1)) + ("mode" -> "le"))

    case CSPOMConstraint(r, 'gt, Seq(a, b), p) =>
      CSPOMConstraint(r, 'sum, Seq(CSPOMSeq(a, b), CSPOMConstant(0)),
        p + ("coefficients" -> Seq(-1, 1)) + ("mode" -> "lt"))

    case CSPOMConstraint(r, 'ge, Seq(a, b), p) =>
      CSPOMConstraint(r, 'sum, Seq(CSPOMSeq(a, b), CSPOMConstant(0)),
        p + ("coefficients" -> Seq(-1, 1)) + ("mode" -> "le"))

    case CSPOMConstraint(r, 'or, a, p) =>
      CSPOMConstraint(r, 'clause, Seq(CSPOMSeq(a: _*), CSPOMSeq()), p)

  }
}