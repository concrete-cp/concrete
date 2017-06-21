package concrete.generator.cspompatterns

import cspom.CSPOM.{constant, constantSeq, seq2CSPOMSeq}
import cspom.compiler.{ConstraintCompiler, ConstraintCompilerNoData, Delta, GlobalCompiler}
import cspom.variable._
import cspom.{CSPOM, CSPOMConstraint}

object XCSPPatterns {
  val mtch: PartialFunction[CSPOMConstraint[_], CSPOMConstraint[_]] = {
    case CSPOMConstraint(a, 'sub, Seq(b, c), p) =>
      CSPOMConstraint('sum)(Seq(-1, 1, -1), Seq(a, b, c), 0) withParams p + ("mode" -> "eq")
    //      linear(Seq((-1, coerce(a)), (1, coerce(b)), (-1, coerce(c))), "eq", 0) withParams p

    case CSPOMConstraint(a, 'add, args, p) =>
      CSPOMConstraint('sum)(-1 +: Seq.fill(args.size)(1), a +: args, 0) withParams p + ("mode" -> "eq")

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

    case CSPOMConstraint(r, 'iff, Seq(x, y), p) => {
      CSPOMConstraint(r)('eq)(x, y) withParams p
    }

    case CSPOMConstraint(r, 'imp, Seq(x, y), p) => {
      CSPOMConstraint(r)('clause)(CSPOMSeq(y), CSPOMSeq(x)) withParams p
    }
  }

  val mtchMulti: PartialFunction[CSPOMConstraint[_], Seq[CSPOMConstraint[_]]] = {
    case CSPOMConstraint(CSPOMConstant(true), 'alldifferentList, lists, _) => {
      for (Seq(a1, a2) <- lists.combinations(2).toSeq) yield {
        CSPOMConstraint('nevec)(a1, a2)
      }
    }

    case CSPOMConstraint(CSPOMConstant(true), 'alldifferentMatrix, Seq(CSPOMSeq(matrix)), _) => {
      val seqMat: Seq[Seq[CSPOMExpression[_]]] = matrix
        .map { case CSPOMSeq(line) => line }

      val transposed: Seq[Seq[CSPOMExpression[_]]] = seqMat.transpose

      (seqMat ++ transposed).map(c =>
        CSPOMConstraint('alldifferent)(c: _*)
      )
    }

    case CSPOMConstraint(CSPOMConstant(true), 'channelBool, Seq(list: CSPOMSeq[_], value), _) => {
      //      val exactly = CSPOMDriver.pseudoBoolean(
      //        list.values.asInstanceOf[Seq[SimpleExpression[Boolean]]],
      //        Seq.fill(list.size)(1), "eq", 1)

      val reif = for ((v, i) <- list.zipWithIndex) yield {
        CSPOMConstraint(v)('eq)(value, CSPOMConstant(i))
      }

      // exactly +:
      reif.toSeq

    }


  }

  def apply() = Seq(
    Ordered, Lex, NonLinearSum,
    new GlobalCompiler(mtch) {
      def selfPropagation = true
    },
    new ConstraintCompiler {
      override def constraintMatcher = mtchMulti

      type A = Seq[CSPOMConstraint[_]]

      def compile(c: CSPOMConstraint[_], problem: CSPOM, data: A) = {
        replaceCtr(c, data, problem)
      }
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

