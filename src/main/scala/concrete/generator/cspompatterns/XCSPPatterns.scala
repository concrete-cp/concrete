package concrete.generator.cspompatterns

import concrete.CSPOMDriver._
import cspom.CSPOM
import cspom.CSPOMConstraint
import cspom.compiler.ConstraintCompiler
import cspom.compiler.ConstraintCompilerNoData
import cspom.compiler.Delta
import cspom.compiler.GlobalCompiler
import cspom.compiler.Types
import cspom.compiler.VariableCompiler
import cspom.variable.BoolVariable
import cspom.variable.CSPOMSeq
import cspom.variable.IntExpression
import cspom.variable.CSPOMVariable
import cspom.variable.CSPOMExpression
import CSPOM._
import cspom.variable.BoolExpression

object XCSPPatterns {
  def apply() = Seq(
    XCSPTypes,
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
    case CSPOMConstraint(IntExpression(a), 'sub, Seq(IntExpression(b), IntExpression(c)), p) =>
      linear(Seq((-1, a), (1, b), (-1, c)), "eq", 0) withParams p

    case CSPOMConstraint(IntExpression(a), 'add, Seq(IntExpression(b), IntExpression(c)), p) =>
      linear(Seq((-1, a), (1, b), (1, c)), "eq", 0) withParams p

    case CSPOMConstraint(r, 'lt, Seq(a, b), p) =>
      CSPOMConstraint(r)('sum)(Seq(a, b), 0) withParams p + ("coefficients" -> Seq(1, -1), "mode" -> "lt")

    case CSPOMConstraint(r, 'le, Seq(a, b), p) =>
      CSPOMConstraint(r)('sum)(Seq(a, b), 0) withParams p + ("coefficients" -> Seq(1, -1), "mode" -> "le")

    case CSPOMConstraint(r, 'gt, Seq(a, b), p) =>
      CSPOMConstraint(r)('sum)(Seq(a, b), 0) withParams p + ("coefficients" -> Seq(-1, 1), "mode" -> "lt")

    case CSPOMConstraint(r, 'ge, Seq(a, b), p) =>
      CSPOMConstraint(r)('sum)(Seq(a, b), 0) withParams p + ("coefficients" -> Seq(-1, 1), "mode" -> "le")

    case CSPOMConstraint(r, 'or, a, p) =>
      CSPOMConstraint(r)('clause)(CSPOMSeq(a: _*), CSPOMSeq()) withParams p

    case CSPOMConstraint(r, 'allDifferent, a, p) =>
      CSPOMConstraint(r)('alldifferent)(a: _*) withParams p
  }
}

object XCSPTypes extends Types {

  def types = {
    case CSPOMConstraint(a, 'sub, Seq(b, c), _) => Map(
      a -> IntExpression.coerce(a),
      b -> IntExpression.coerce(b),
      c -> IntExpression.coerce(c))

    case CSPOMConstraint(a, 'add, Seq(b, c), _) => Map(
      a -> IntExpression.coerce(a),
      b -> IntExpression.coerce(b),
      c -> IntExpression.coerce(c))

    case CSPOMConstraint(r, 'and, a, _) => Map(
      r -> BoolExpression.coerce(r)) ++
      a.map(l => l -> BoolExpression.coerce(l))

  }

}