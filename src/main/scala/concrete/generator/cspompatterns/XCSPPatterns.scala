package concrete.generator.cspompatterns

import cspom.CSPOM.{constant, constantSeq, seq2CSPOMSeq}
import cspom.{CSPOM, CSPOMConstraint}
import cspom.compiler.{ConstraintCompilerNoData, Delta, GlobalCompiler}
import cspom.variable.{BoolVariable, SimpleExpression}

object XCSPPatterns {
  val mtch: PartialFunction[CSPOMConstraint[_], CSPOMConstraint[_]] = {
    case CSPOMConstraint(a, 'sub, Seq(b, c), p) =>
      CSPOMConstraint('sum)(Seq(-1, 1, -1), Seq(a, b, c), 0) withParams p + ("mode" -> "eq")
    //      linear(Seq((-1, coerce(a)), (1, coerce(b)), (-1, coerce(c))), "eq", 0) withParams p

    case CSPOMConstraint(a, 'add, Seq(b, c), p) =>
      CSPOMConstraint('sum)(Seq(-1, 1, 1), Seq(a, b, c), 0) withParams p + ("mode" -> "eq")

    case CSPOMConstraint(r, 'ne, Seq(a, b), p) =>
      CSPOMConstraint(r)('sum)(Seq(1, -1), Seq(a, b), 0) withParams p + ("mode" -> "ne")

    case CSPOMConstraint(r, 'lt, Seq(a, b), p) =>
      CSPOMConstraint(r)('sum)(Seq(1, -1), Seq(a, b), 0) withParams p + ("mode" -> "lt")

    case CSPOMConstraint(r, 'le, Seq(a, b), p) =>
      CSPOMConstraint(r)('sum)(Seq(1, -1), Seq(a, b), 0) withParams p + ("mode" -> "le")

    case CSPOMConstraint(r, 'gt, Seq(a, b), p) =>
      CSPOMConstraint(r)('sum)(Seq(-1, 1), Seq(a, b), 0) withParams p + ("mode" -> "lt")

    case CSPOMConstraint(r, 'ge, Seq(a, b), p) =>
      CSPOMConstraint(r)('sum)(Seq(-1, 1), Seq(a, b), 0) withParams p + ("mode" -> "le")

    case CSPOMConstraint(r, 'or, a, p) =>
      CSPOMConstraint(r)('clause)(a, Seq[SimpleExpression[Boolean]]()) withParams p

    case CSPOMConstraint(r, 'dist, a, p) =>
      CSPOMConstraint(r)('absdiff)(a: _*) withParams p

    case CSPOMConstraint(r, 'allDifferent, a, p) =>
      CSPOMConstraint(r)('alldifferent)(a: _*) withParams p

    case CSPOMConstraint(r, 'set, vars, p) =>
      CSPOMConstraint('member)(vars, r) withParams p

    /** Semantics of "in" in XCSP3 and Concrete differ */
    case CSPOMConstraint(r, 'in, Seq(x, y: SimpleExpression[_]), p) =>
      CSPOMConstraint(r)('eq)(x, y) withParams p
  }

  def apply() = Seq(
    Ordered, Lex,
    new GlobalCompiler(mtch) {
      def selfPropagation = true
    })

  object Ordered extends ConstraintCompilerNoData {
    def matchBool(constraint: CSPOMConstraint[_], problem: CSPOM): Boolean = constraint.function == 'ordered

    def compile(constraint: CSPOMConstraint[_], problem: CSPOM): Delta = {
      val mode = Symbol(constraint.getParam[String]("mode").get.toLowerCase)
      val slide = constraint.arguments.sliding(2)
        .map { s =>
          CSPOMConstraint(new BoolVariable())(mode)(s: _*)
        }
        .toSeq
      val reif = CSPOMConstraint(constraint.result)('and)(slide.map(_.result): _*)
      replaceCtr(constraint, reif +: slide, problem)
    }

  }

}

