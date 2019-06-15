package concrete.generator.cspompatterns

import concrete.constraint.linear.SumMode
import cspom.CSPOM._
import concrete.CSPOMDriver._
import cspom.compiler.ConstraintCompiler._
import cspom.compiler._
import cspom.variable._
import cspom.{CSPOM, CSPOMConstraint}

object XCSPPatterns {
  def compilers: Seq[(String, PartialFunction[CSPOMConstraint[_], CSPOMConstraint[_]])] = Seq(
    "sub" -> { case CSPOMConstraint(a, "sub", Seq(b, c), p) =>
      CSPOMConstraint("sum")(Seq(-1, 1, -1), Seq(a, b, c), 0) withParams p + ("mode" -> "eq")
    },
    //      linear(Seq((-1, coerce(a)), (1, coerce(b)), (-1, coerce(c))), "eq", 0) withParams p

    "add" -> { case CSPOMConstraint(a, "add", args, p) =>
      CSPOMConstraint("sum")(-1 +: Seq.fill(args.size)(1), a +: args, 0) withParams p + ("mode" -> "eq")
    },

    //    case CSPOMConstraint(r, "ne", Seq(a, b), p) =>
    //      CSPOMConstraint(r)("sum")(Seq(1, -1), Seq(a, b), 0) withParams p + ("mode" -> "ne")

    "lt" -> { case CSPOMConstraint(r, "lt", Seq(a, b), p) =>
      CSPOMConstraint(r)("sum")(Seq(1, -1), Seq(a, b), 0) withParams p + ("mode" -> "lt")
    },

    "le" -> { case CSPOMConstraint(r, "le", Seq(a, b), p) =>
      CSPOMConstraint(r)("sum")(Seq(1, -1), Seq(a, b), 0) withParams p + ("mode" -> "le")
    },

    "gt" -> { case CSPOMConstraint(r, "gt", Seq(a, b), p) =>
      CSPOMConstraint(r)("sum")(Seq(-1, 1), Seq(a, b), 0) withParams p + ("mode" -> "lt")
    },

    "ge" -> { case CSPOMConstraint(r, "ge", Seq(a, b), p) =>
      CSPOMConstraint(r)("sum")(Seq(-1, 1), Seq(a, b), 0) withParams p + ("mode" -> SumMode.LE)
    },

    "or" -> { case CSPOMConstraint(r, "or", a, p) =>
      CSPOMConstraint(r)("clause")(a, Seq[SimpleExpression[Boolean]]()) withParams p
    },

    "dist" -> { case CSPOMConstraint(r, "dist", a, p) =>
      CSPOMConstraint(r)("absdiff")(a: _*) withParams p
    },

    "allDifferent" -> { case CSPOMConstraint(r, "allDifferent", a, p) =>
      CSPOMConstraint(r)("alldifferent")(a: _*) withParams p
    },

    "set" -> { case CSPOMConstraint(r, "set", vars, p) =>
      CSPOMConstraint("member")(vars, r) withParams p
    },

    /** Semantics of "in" in XCSP3 and Concrete differ */
    "in" -> { case CSPOMConstraint(r, "in", Seq(x, y: SimpleExpression[_]), p) =>
      CSPOMConstraint(r)("eq")(x, y) withParams p
    },

    "iff" -> { case CSPOMConstraint(r, "iff", Seq(x, y), p) =>
      CSPOMConstraint(r)("eq")(x, y) withParams p
    },

    "imp" -> { case CSPOMConstraint(r, "imp", Seq(x, y), p) =>
      CSPOMConstraint(r)("clause")(CSPOMSeq(y), CSPOMSeq(x)) withParams p
    },

    "neg" -> { case CSPOMConstraint(r, "neg", Seq(x), p) =>
      CSPOMConstraint("sum")(Seq(1, 1), Seq(r, x), 0) withParams p + ("mode" -> SumMode.EQ)
    })


  def compilersMulti: Seq[(String, PartialFunction[CSPOMConstraint[_], Seq[CSPOMConstraint[_]]])] = Seq(
    "alldifferentList" -> { case CSPOMConstraint(CSPOMConstant(true), "alldifferentList", lists, _) =>
      for (Seq(a1, a2) <- lists.combinations(2).toSeq) yield {
        CSPOMConstraint("nevec")(a1, a2)
      }
    },

    "alldifferentMatrix" -> { case CSPOMConstraint(CSPOMConstant(true), "alldifferentMatrix", Seq(CSPOMSeq(matrix)), _) =>
      val seqMat: Seq[Seq[CSPOMExpression[_]]] = matrix
        .map { case CSPOMSeq(line) => line }

      val transposed: Seq[Seq[CSPOMExpression[_]]] = seqMat.transpose

      (seqMat ++ transposed).map(c =>
        CSPOMConstraint("alldifferent")(c: _*)
      )
    },

    "channelBool" -> { case CSPOMConstraint(CSPOMConstant(true), "channelBool", Seq(list: CSPOMSeq[_], value), _) =>
      //      val exactly = CSPOMDriver.pseudoBoolean(
      //        list.values.asInstanceOf[Seq[SimpleExpression[Boolean]]],
      //        Seq.fill(list.size)(1), "eq", 1)

      val reif = for ((v, i) <- list.zipWithIndex) yield {
        CSPOMConstraint(v)("eq")(value, CSPOMConstant(i))
      }

      // exactly +:
      reif.toSeq
    }
  )

  def apply(): Seq[ConstraintCompiler] = Seq(
    Ordered, Lex, NonLinearSum) ++
    compilers.map { case (symbol, function) => new GlobalCompiler {
      val functions: Functions = Functions(symbol)
      override val constraintMatcher: PartialFunction[CSPOMConstraint[_], Seq[CSPOMConstraint[_]]] = function.andThen(Seq(_))
    }
    } ++
    compilersMulti.map { case (symbol, function) => new GlobalCompiler {
      val functions = Functions(symbol)

      override def constraintMatcher: PartialFunction[CSPOMConstraint[_], Seq[CSPOMConstraint[_]]] = function
    }
    }


  object Ordered extends ConstraintCompilerNoData {
    def functions = Functions("ordered")

    def matchBool(constraint: CSPOMConstraint[_], problem: CSPOM): Boolean = true

    def compile(constraint: CSPOMConstraint[_], problem: CSPOM): Delta = {
      val Seq(IntExpression.simpleSeq(variables), IntExpression.simpleSeq(lengths)) = constraint.arguments

      require(variables.length == lengths.length + 1)

      if (variables.lengthCompare(1) <= 0) {
        removeCtr(constraint, problem)
      } else {
        val mode = constraint.getParam[String]("mode").get.toLowerCase

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

        val reif = CSPOMConstraint(constraint.result)("and")(r: _*)
        replaceCtr(constraint, reif +: shift ++: ord, problem)
      }
    }

  }

}

