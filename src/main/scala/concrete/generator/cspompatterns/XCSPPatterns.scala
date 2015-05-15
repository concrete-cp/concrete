package concrete.generator.cspompatterns

import cspom.CSPOMConstraint
import cspom.compiler.GlobalCompiler
import cspom.variable.CSPOMConstant
import cspom.compiler.Ctr
import cspom.variable.CSPOMSeq

object XCSPPatterns {
  def apply() = new GlobalCompiler(mtch) { def selfPropagation = true }

  //  val debug = new PartialFunction[CSPOMConstraint[_], CSPOMConstraint[_]] {
  //    def isDefinedAt(c: CSPOMConstraint[_]) = {
  //      println(c)
  //      false
  //    }
  //
  //    def apply(c: CSPOMConstraint[_]) = sys.error("")
  //  }

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