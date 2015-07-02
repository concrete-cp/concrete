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
import concrete.CSPOMDriver._
import cspom.variable.IntExpression

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
    case CSPOMConstraint(IntExpression(a), 'sub, Seq(IntExpression(b), IntExpression(c)), p) =>
      linear(Seq((-1, a), (1, b), (-1, c)), "eq", 0) withParams p

    case CSPOMConstraint(IntExpression(a), 'add, Seq(IntExpression(b), IntExpression(c)), p) =>
      linear(Seq((-1, a), (1, b), (1, c)), "eq", 0) withParams p

    case CSPOMConstraint(r, 'lt, Seq(IntExpression(a), IntExpression(b)), p) =>
      linear(Seq((1, a), (-1, b)), "lt", 0) withParams p

    case CSPOMConstraint(r, 'le, Seq(IntExpression(a), IntExpression(b)), p) =>
      linear(Seq((1, a), (-1, b)), "le", 0) withParams p
      
    case CSPOMConstraint(r, 'gt, Seq(IntExpression(a), IntExpression(b)), p) =>
      linear(Seq((-1, a), (1, b)), "lt", 0) withParams p
      
    case CSPOMConstraint(r, 'ge, Seq(IntExpression(a), IntExpression(b)), p) =>
      linear(Seq((-1, a), (1, b)), "le", 0) withParams p

    case CSPOMConstraint(r, 'or, a, p) =>
      CSPOMConstraint(r, 'clause, Seq(CSPOMSeq(a: _*), CSPOMSeq()), p)

  }
}