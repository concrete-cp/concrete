package concrete.generator.cspompatterns

import concrete.constraint.linear.{SumEQ, SumLE}
import cspom.CSPOM._
import concrete.CSPOMDriver._
import cspom.compiler.ConstraintCompiler._
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

    //    case CSPOMConstraint(r, 'ne, Seq(a, b), p) =>
    //      CSPOMConstraint(r)('sum)(Seq(1, -1), Seq(a, b), 0) withParams p + ("mode" -> "ne")

    case CSPOMConstraint(r, 'lt, Seq(a, b), p) =>
      CSPOMConstraint(r)('sum)(Seq(1, -1), Seq(a, b), 0) withParams p + ("mode" -> "lt")

    case CSPOMConstraint(r, 'le, Seq(a, b), p) =>
      CSPOMConstraint(r)('sum)(Seq(1, -1), Seq(a, b), 0) withParams p + ("mode" -> "le")

    case CSPOMConstraint(r, 'gt, Seq(a, b), p) =>
      CSPOMConstraint(r)('sum)(Seq(-1, 1), Seq(a, b), 0) withParams p + ("mode" -> "lt")

    case CSPOMConstraint(r, 'ge, Seq(a, b), p) =>
      CSPOMConstraint(r)('sum)(Seq(-1, 1), Seq(a, b), 0) withParams p + ("mode" -> SumLE)

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

    case CSPOMConstraint(r, 'neg, Seq(x), p) => {
      CSPOMConstraint('sum)(Seq(1, 1), Seq(r, x), 0) withParams p + ("mode" -> SumEQ)
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
      override def constraintMatcher: PartialFunction[CSPOMConstraint[_], Seq[CSPOMConstraint[_]]] = mtchMulti

      type A = Seq[CSPOMConstraint[_]]

      def compile(c: CSPOMConstraint[_], problem: CSPOM, data: A): Delta = {
        replaceCtr(c, data, problem)
      }
    })

  object Ordered extends ConstraintCompilerNoData {
    def matchBool(constraint: CSPOMConstraint[_], problem: CSPOM): Boolean = constraint.function == 'ordered

    def compile(constraint: CSPOMConstraint[_], problem: CSPOM): Delta = {
      val Seq(IntExpression.simpleSeq(variables), IntExpression.simpleSeq(lengths)) = constraint.arguments

      require(variables.length == lengths.length + 1)

      if (variables.lengthCompare(1) <= 0) {
        removeCtr(constraint, problem)
      } else {
        val mode = Symbol(constraint.getParam[String]("mode").get.toLowerCase)

        val rewrite = for {
          i <- 1 until variables.length
        } yield {
          val y = IntVariable.free()
          val shift = linear(Seq(variables(i - 1), lengths(i - 1), y), Seq(1, 1, -1), "eq", 0)
          val r = new BoolVariable
          val ord = CSPOMConstraint(r)(mode)(y, variables(i))
          (r, shift, ord)
        }

        val (r, shift, ord) = rewrite.unzip3

        val reif = CSPOMConstraint(constraint.result)('and)(r: _*)
        replaceCtr(constraint, reif +: shift ++: ord, problem)
      }
    }

  }

}

