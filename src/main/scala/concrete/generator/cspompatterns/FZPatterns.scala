package concrete.generator.cspompatterns

import concrete.CSPOMDriver._
import cspom.CSPOM._
import cspom.compiler.{ConstraintCompiler, Ctr, Functions, GlobalCompiler}
import cspom.extension.MDDRelation
import cspom.variable._
import cspom.{CSPOM, CSPOMConstraint}

object FZPatterns {

  def apply(): Seq[ConstraintCompiler] = Seq[(scala.Symbol, PartialFunction[CSPOMConstraint[_], CSPOMConstraint[_]])](
    /**
      * (∀ i ∈ 1..n : as[i]) ↔ r where n is the length of as
      * array_bool_and(array [int] of var bool: as, var bool: r)
      */
    'array_bool_and -> { case Ctr(_, Seq(CSPOMSeq(as), r), p) => CSPOMConstraint(r, 'and, as, p) },

    /**
      * b ∈ 1..n ∧ as[b] = c where n is the length of as
      * array_bool_element(var int: b, array [int] of bool: as, var bool: c)
      */
    'array_bool_element -> { case Ctr(_, Seq(b, as, c), p) => CSPOMConstraint(c, 'element, Seq(as, b), p) },

    /**
      * (∃ i ∈ 1..n : as[i]) ↔ r where n is the length of as
      * array_bool_or(array [int] of var bool: as, var bool: r)
      */
    'array_bool_or -> { case Ctr(_, Seq(as, r), p) => CSPOMConstraint(r, 'clause, Seq(as, CSPOMSeq.empty), p) },

    /**
      * (((i ∈ 1..n : as[i]) mod 2) = 1) where n is the length of as
      * array_bool_xor(array [int] of var bool: as)
      */
    'array_bool_xor -> { case Ctr(_, Seq(CSPOMSeq(as)), p) => CSPOMConstraint('xor)(as: _*) withParams p },

    /**
      * b ∈ 1..n ∧ as[b] = c where n is the length of as
      * array_float_element(var int: b, array [int] of float: as, var float: c)
      */
    /**
      * b ∈ 1..n ∧ as[b] = c where n is the length of as
      * array_int_element(var int: b, array [int] of int: as, var int: c)
      */
    'array_int_element -> { case Ctr(_, Seq(b, as, c), p) => CSPOMConstraint(c, 'element, Seq(as, b), p) },

    /**
      * b ∈ 1..n ∧ as[b] = c where n is the length of as
      * array_set_element(var int: b, array [int] of set of int: as, set of int: c)
      */
    /**
      * b ∈ 1..n ∧ as[b] = c where n is the length of as
      * array_var_bool_element(var int: b, array [int] of var bool: as, var bool: c)
      */
    'array_var_bool_element -> { case Ctr(_, Seq(b, as, c), p) => CSPOMConstraint(c, 'element, Seq(as, b), p) },

    /**
      * b ∈ 1..n ∧ as[b] = c where n is the length of as
      * array_var_float_element(var int: b, array [int] of var float: as, var float: c)
      */
    /**
      * b ∈ 1..n ∧ as[b] = c where n is the length of as
      * array_var_int_element(var int: b, array [int] of var int: as, var int: c)
      */
    'array_var_int_element -> { case Ctr(_, Seq(b, as, c), p) => CSPOMConstraint(c, 'element, Seq(as, b), p) },

    /**
      * b ∈ 1..n ∧ as[b] = c where n is the length of as
      * array_var_set_element(var int: b, array [int] of var set of int: as, var set of int: c)
      */
    /**
      * (a ↔ b = 1) ∧ (¬a ↔ b = 0)
      * bool2int(var bool: a, var int: b)
      */
    'bool2int -> { case Ctr(_, args, p) => CSPOMConstraint('eq)(args: _*) withParams p },

    /**
      * (a ∧ b) ↔ r
      * bool_and(var bool: a, var bool: b, var bool: r)
      */
    'bool_and -> { case Ctr('bool_and, Seq(a, b, r), p) => CSPOMConstraint(r, 'and, Seq(a, b), p) },


    /**
      * (∃ i ∈ 1..nas : as[i]) ∨ (∃ i ∈ 1..nbs : ¬bs[i]) where n is the length of as
      * bool_clause(array [int] of var bool: as, array [int] of var bool: bs)
      */
    /**
      * a = b
      * bool_eq(var bool: a, var bool: b)
      */
    'bool_eq -> { case Ctr(_, a: Seq[_], p) => CSPOMConstraint('eq)(a: _*) withParams p },

    /**
      * (a = b) ↔ r
      * bool_eq_reif(var bool: a, var bool: b, var bool: r)
      */
    'bool_eq_reif -> { case Ctr(_, Seq(a, b, r), p) => new CSPOMConstraint(r, 'eq, Seq(a, b), p) },

    /**
      * ¬a ∨ b
      * bool_le(var bool: a, var bool: b)
      */
    /**
      * (¬a ∨ b) ↔ r
      * bool_le_reif(var bool: a, var bool: b, var bool: r)
      */
    'bool_le_reif -> { case Ctr(_, Seq(a, b, r), p) => new CSPOMConstraint(r, 'clause, Seq(Seq(b), Seq(a)), p) },

    /**
      * (∃ i ∈ 1..nas : as[i]) ∨ (∃ i ∈ 1..nbs : ¬bs[i]) where n is the length of as
      * bool_clause(array [int] of var bool: as, array [int] of var bool: bs)
      */
    'bool_clause -> { case Ctr(_, Seq(as: CSPOMSeq[_], bs: CSPOMSeq[_]), p) =>
      clause(as, bs) withParams p },

    /**
      * ¬a ∨ b
      * bool_le(var bool: a, var bool: b)
      */
    'bool_le -> { case Ctr(_, Seq(BoolExpression.bool01(a), BoolExpression.bool01(b)), p) =>
      clause(b)(a) withParams p },

    /**
      * (¬a /\ b) ↔ r
      * bool_lt_reif(var bool: a, var bool: b, var bool: r)
      */
    'bool_lt_reif -> { case Ctr(_, Seq(a, b, r), p) =>
      CSPOMConstraint(r)('sum)(Seq(1, -1), Seq(a, b), CSPOMConstant(0)) withParams p + ("mode" -> "lt")
    },

    /**
      * i ∈ 1..n : as[i].bs[i] = c where n is the common length of as and bs
      * bool_lin_eq(array [int] of int: as, array [int] of var bool: bs, var int: c)
      */
    'bool_lin_eq -> { case Ctr(_, Seq(as: CSPOMSeq[_], bs: CSPOMSeq[_], CSPOMConstant(c: Int)), p) =>
      pseudoBoolean(bs, as, "eq", c) withParams p
    },

    /**
      * i ∈ 1..n : as[i].bs[i] ≤ c where n is the common length of as and bs
      * bool_lin_le(array [int] of int: as, array [int] of var bool: bs, int: c)
      */
    'bool_lin_le -> { case Ctr(_, Seq(as: CSPOMSeq[_], bs: CSPOMSeq[_], CSPOMConstant(c: Int)), p) =>
      pseudoBoolean(bs, as, "le", c) withParams p
    },

    /**
      * ¬a ∧ b
      * bool_lt(var bool: a, var bool: b)
      */
    /**
      * (¬a ∧ b) ↔ r
      * bool_lt_reif(var bool: a, var bool: b, var bool: r)
      */
    /**
      * ¬a = b
      * bool_not(var bool: a, var bool: b)
      */
    /**
      * (a ∨ b) ↔ r
      * bool_or(var bool: a, var bool: b, var bool: r)
      */
    /**
      * (a = b) ↔ r
      * bool_xor(var bool: a, var bool: b, var bool: r)
      */
    'bool_xor -> { case Ctr(_, Seq(a, b, r), p) => CSPOMConstraint(r, 'xor, Seq(a, b), p) },

    /**
      * count_eq(array[int] of var int: x, var int: y, var int: c)
      *
      */
    'count_eq -> { case Ctr(_, Seq(x, y, c), p) => CSPOMConstraint(c, 'occurrence, Seq(y, x), p) },

    'count_le -> { case Ctr(_, Seq(x, y, c), p) => CSPOMConstraint('atMost)(c, y, x) withParams p },

    /**
      * |a| = b
      * float_abs(var float: a, var float: b)
      */
    /**
      * acos a = b
      * float_acos(var float: a, var float: b)
      */
    /**
      * asin a = b
      * float_asin(var float: a, var float: b)
      */
    /**
      * atan a = b
      * float_atan(var float: a, var float: b)
      */
    /**
      * cos a = b
      * float_cos(var float: a, var float: b)
      */
    /**
      * cosh a = b
      * float_cosh(var float: a, var float: b)
      */
    /**
      * exp a = b
      * float_exp(var float: a, var float: b)
      */
    /**
      * ln a = b
      * float_ln(var float: a, var float: b)
      */
    /**
      * log 10 a = b
      * float_log10(var float: a, var float: b)
      */
    /**
      * log 2 a = b
      * float_log2(var float: a, var float: b)
      */
    /**
      * √a = b
      * float_sqrt(var float: a, var float: b)
      */
    /**
      * sin a = b
      * float_sin(var float: a, var float: b)
      */
    /**
      * sinh a = b
      * float_sinh(var float: a, var float: b)
      */
    /**
      * tan a = b
      * float_tan(var float: a, var float: b)
      */
    /**
      * tanh a = b
      * float_tanh(var float: a, var float: b)
      */
    /**
      * a = b
      * float_eq(var float: a, var float: b)
      */
    /**
      * (a = b) ↔ r
      * float_eq_reif(var float: a, var float: b, var bool: r)
      */
    /**
      * a ≤ b
      * float_le(var float: a, var float: b)
      */
    /**
      * (a ≤ b) ↔ r
      * float_le_reif(var float: a, var float: b, var bool: r)
      */
    /**
      * i ∈ 1..n : as[i].bs[i] = c where n is the common length of as and bs
      * float_lin_eq(array [int] of float: as, array [int] of var float: bs, float: c)
      */
    /**
      * (i ∈ 1..n : as[i].bs[i] = c) ↔ r where n is the common length of as and bs
      * float_lin_eq_reif(array [int] of float: as, array [int] of var float: bs, float: c, var bool: r)
      */
    /**
      * i ∈ 1..n : as[i].bs[i] ≤ c where n is the common length of as and bs
      * float_lin_le(array [int] of float: as, array [int] of var float: bs, float: c)
      */
    /**
      * (i ∈ 1..n : as[i].bs[i] ≤ c) ↔ r where n is the common length of as and bs
      * float_lin_le_reif(array [int] of float: as, array [int] of var float: bs, float: c, var bool: r)
      */
    /**
      * i ∈ 1..n : as[i].bs[i] < c where n is the common length of as and bs
      * float_lin_lt(array [int] of float: as, array [int] of var float: bs, float: c)
      */
    /**
      * (i ∈ 1..n : as[i].bs[i] < c) ↔ r where n is the common length of as and bs
      * float_lin_lt_reif(array [int] of float: as, array [int] of var float: bs, float: c, var bool: r)
      */
    /**
      * i ∈ 1..n : as[i].bs[i] = c where n is the common length of as and bs
      * float_lin_ne(array [int] of float: as, array [int] of var float: bs, float: c)
      */
    /**
      * (i ∈ 1..n : as[i].bs[i] = c) ↔ r where n is the common length of as and bs
      * float_lin_ne_reif(array [int] of float: as, array [int] of var float: bs, float: c, var bool: r)
      */
    /**
      * a < b
      * float_lt(var float: a, var float: b)
      */
    /**
      * (a < b) ↔ r
      * float_lt_reif(var float: a, var float: b, var bool: r)
      */
    /**
      * max(a, b) = c
      * float_max(var float: a, var float: b, var float: c)
      */
    /**
      * min(a, b) = c
      * float_min(var float: a, var float: b, var float: c)
      */
    /**
      * a = b
      * float_ne(var float: a, var float: b)
      */
    /**
      * (a = b) ↔ r
      * float_ne_reif(var float: a, var float: b, var bool: r)
      */
    /**
      * a+b = c
      * float_plus(var float: a, var float: b, var float: c)
      */
    /**
      * |a| = b
      * int_abs(var int: a, var int: b)
      */
    'int_abs -> { case Ctr(_, Seq(a, b), p) => CSPOMConstraint(b, 'abs, Seq(a), p) },

    /**
      * a/b = c rounding towards zero.
      * int_div(var int: a, var int: b, var int: c)
      */
    'int_div -> { case Ctr(_, Seq(a, b, c), p) => CSPOMConstraint(c, 'div, Seq(a, b), p) },

    /**
      * a = b
      * int_eq(var int: a, var int: b)
      */
    'int_eq -> { case Ctr(_, args, p) => CSPOMConstraint('eq)(args: _*) withParams p },

    /**
      * (a = b) ↔ r
      * int_eq_reif(var int: a, var int: b, var bool: r)
      */
    'int_eq_reif -> { case Ctr(_, Seq(a, b, r), p) => CSPOMConstraint(r, 'eq, Seq(a, b), p) },

    /**
      * a ≤ b
      * int_le(var int: a, var int: b)
      */
    'int_le -> { case Ctr(_, Seq(IntExpression(a), IntExpression(b)), p) => 1 *: a + -1 *: b <= 0 withParams p },

    /**
      * (a ≤ b) ↔ r
      * int_le_reif(var int: a, var int: b, var bool: r)
      */
    'int_le_reif -> { case Ctr(_, Seq(a, b, r), p) =>
      CSPOMConstraint(r)('sum)(Seq(1, -1), Seq(a, b), CSPOMConstant(0)) withParams (p + ("mode" -> "le"))
    },

    /**
      * i ∈ 1..n : as[i].bs[i] = c where n is the common length of as and bs
      * int_lin_eq(array [int] of int: as, array [int] of var int: bs, int: c)
      */
    'int_lin_eq -> { case Ctr(_, Seq(IntExpression.constSeq(as), IntExpression.simpleSeq(bs), CSPOMConstant(c: Int)), p) =>
      linear(bs, as, "eq", c) withParams p
    },

    /**
      * i ∈ 1..n : as[i].bs[i] = c) ↔ r where n is the common length of as and bs
      * int_lin_eq_reif(array [int] of int: as, array [int] of var int: bs, int: c, var bool: r)
      */
    'int_lin_eq_reif -> { case Ctr(_, Seq(as, bs, c, r), p) =>
      CSPOMConstraint(r)('sum)(as, bs, c) withParams (p + ("mode" -> "eq"))
    },

    /**
      * i ∈ 1..n : as[i].bs[i] ≤ c where n is the common length of as and bs
      * int_lin_le(array [int] of int: as, array [int] of var int: bs, int: c)
      */
    'int_lin_le -> { case Ctr(_, Seq(IntExpression.constSeq(as), IntExpression.simpleSeq(bs), CSPOMConstant(c: Int)), p) =>
      linear(bs, as, "le", c) withParams p
    },

    /**
      * (i ∈ 1..n : as[i].bs[i] ≤ c) ↔ r where n is the common length of as and bs
      * int_lin_le_reif(array [int] of int: as, array [int] of var int: bs, int: c, var bool: r)
      */
    'int_lin_le_reif -> { case Ctr(_, Seq(as, bs, c, r), p) =>
      CSPOMConstraint(r)('sum)(as, bs, c) withParams p + ("mode" -> "le")
    },

    /**
      * i ∈ 1..n : as[i].bs[i] = c where n is the common length of as and bs
      * int_lin_ne(array [int] of int: as, array [int] of var int: bs, int: c)
      */
    'int_lin_ne -> { case Ctr(_, Seq(IntExpression.constSeq(as), IntExpression.simpleSeq(bs), CSPOMConstant(c: Int)), p) =>
      linear(bs, as, "ne", c) withParams p
    },

    /**
      * (i ∈ 1..n : as[i].bs[i] = c) ↔ r where n is the common length of as and bs
      * int_lin_ne_reif(array [int] of int: as, array [int] of var int: bs, int: c, var bool: r)
      */
    'int_lin_ne_reif -> { case Ctr(_, Seq(as, bs, c, r), p) =>
      CSPOMConstraint(r)('sum)(as, bs, c) withParams p + ("mode" -> "ne")
    },

    /**
      * a < b
      * int_lt(var int: a, var int: b)
      */
    'int_lt -> { case Ctr(_, Seq(IntExpression(a), IntExpression(b)), p) => 1 *: a + -1 *: b < 0 withParams p },

    /**
      * (a < b) ↔ r
      * int_lt_reif(var int: a, var int: b, var bool: r)
      */
    'int_lt_reif -> { case Ctr(_, Seq(a, b, r), p) =>
      CSPOMConstraint(r)('sum)(Seq(1, -1), Seq(a, b), CSPOMConstant(0)) withParams p + ("mode" -> "lt")
    },

    /**
      * max(a, b) = c
      * int_max(var int: a, var int: b, var int: c)
      */
    'int_max -> { case Ctr(_, Seq(a: SimpleExpression[_], b: SimpleExpression[_], c: SimpleExpression[_]), p) =>
      CSPOMConstraint(c, 'max, Seq(a, b), p)
    },

    /**
      * min(a, b) = c
      * int_min(var int: a, var int: b, var int: c)
      */
    'int_min -> { case Ctr(_, Seq(a: SimpleExpression[_], b: SimpleExpression[_], c: SimpleExpression[_]), p) =>
      CSPOMConstraint(c, 'min, Seq(a, b), p)
    },

    /**
      * predicate array_int_minimum(var int: m, array[int] of var int: x)
      */
    'array_int_minimum -> { case Ctr(_, Seq(m, x: CSPOMSeq[_]), p) =>
      CSPOMConstraint(m, 'min, x.values, p)
    },

    /**
      * predicate array_int_maximum(var int: m, array[int] of var int: x)
      */
    'array_int_maximum -> { case Ctr(_, Seq(m, x: CSPOMSeq[_]), p) =>
      CSPOMConstraint(m, 'max, x.values, p)
    },

    /**
      * a − x.b = c where x = a/b rounding towards zero.
      * int_mod(var int: a, var int: b, var int: c)
      */
    'int_mod -> { case Ctr(_, Seq(a, b, c), p) => CSPOMConstraint(c, 'mod, Seq(a, b), p) },

    /**
      * a = b
      * int_ne(var int: a, var int: b)
      */
    'int_ne -> { case Ctr(_, Seq(a, b), p) => CSPOMConstraint(CSPOMConstant(false), 'eq, Seq(a, b), p) },

    /**
      * (a = b) ↔ r
      * int_ne_reif(var int: a, var int: b, var bool: r)
      */
    'int_ne_reif -> { case Ctr(_, Seq(a, b, r), p) => CSPOMConstraint(r, 'ne, Seq(a, b), p) },

    /**
      * a+b = c
      * int_plus(var int: a, var int: b, var int: c)
      */
    'int_plus -> { case Ctr(_, Seq(IntExpression(a), IntExpression(b), IntExpression(c)), p) => 1 *: a + 1 *: b + -1 *: c === 0 withParams p },

    /**
      * a×b = c
      * int_times(var int: a, var int: b, var int: c)
      */
    'int_times -> { case Ctr(_, Seq(a, b, c), p) => new CSPOMConstraint(c, 'mul, Seq(a, b), p) },

    /**
      * a = b
      * int2float(var int: a, var float: b)
      */
    /**
      * |a| = b
      * set_card(var set of int: a, var int: b)
      */
    /**
      * a−b = c
      * set_diff(var set of int: a, var set of int: b, var set of int: c)
      */
    /**
      * a = b
      * set_eq(var set of int: a, var set of int: b)
      */
    /**
      * (a = b) ↔ r
      * set_eq_reif(var set of int: a, var set of int: b, var bool: r)
      */
    /**
      * a∈b
      * set_in(var int: a, var set of int: b)
      */

    /**
      * (a ∈ b) ↔ r
      * set_in_reif(var int: a, var set of int: b, var bool: r)
      */
    'set_in_reif -> { case Ctr(_, Seq(a, CSPOMConstant(b: Seq[_]), r), p) =>
      new CSPOMConstraint(r, 'in, Seq(a, b.map {
        case i: Int => CSPOMConstant(i)
      }), p)
    }
    ,

    /**
      * a∩b = c
      * set_intersect(var set of int: a, var set of int: b, var set of int: c)
      */
    /**
      * a ⊆ b ∨ min(a b) ∈ a
      * set_le(var set of int: a, var set of int: b)
      */
    /**
      * a ⊂ b ∨ min(a b) ∈ a
      * set_lt(var set of int: a, var set of int: b)
      */
    /**
      * a = b
      * set_ne(var set of int: a, var set of int: b)
      */
    /**
      * (a = b) ↔ r
      * set_ne_reif(var set of int: a, var set of int: b, var bool: r)
      */
    /**
      * a⊆b
      * set_subset(var set of int: a, var set of int: b)
      */
    /**
      * (a ⊆ b) ↔ r
      * set_subset_reif(var set of int: a, var set of int: b, var bool: r)
      */
    /**
      * a b = c
      * set_symdiff(var set of int: a, var set of int: b, var set of int: c)
      */
    /**
      * a∪b = c
      * set_union(var set of int: a, var set of int: b, var set of int: c)
      */

    /**
      * FlatZinc flattens t.
      * predicate table_int(array[int] of var int: x, array[int, int] of int: t)
      */
    'table_int -> { case Ctr(_, Seq(IntExpression.simpleSeq(x), IntExpression.constSeq(t)), p) =>
      x in MDDRelation(t.grouped(x.size).map(_.toArray).toSeq)
    },

    /**
      * predicate all_different_int(array[int] of var int: x);
      */
    'all_different_int -> { case Ctr(_, Seq(IntExpression.simpleSeq(y)), p) => allDifferent(y: _*) withParams p },

    'regular -> {
      case Ctr(_, Seq(x,
      CSPOMConstant(q: Int),
      CSPOMConstant(s: Int),
      IntExpression.constSeq(fd), //: CSPOMSeq[_],
      q0,
      CSPOMConstant(fseq: Seq[Int])), p) =>
        CSPOMConstraint('regular)(x, q0, CSPOM.constantSeq(fseq)) withParams p + ("dfa" -> dfa(q, s, fd.toIndexedSeq))
    },

    'lex_lesseq_bool -> { case Ctr(_, Seq(x, y), p) => CSPOMConstraint('lexleq)(x, y) withParams p },

    'member_int -> { case Ctr(_, Seq(s, x), p) => CSPOMConstraint('member)(s, x) withParams p },

    'member_int_reif -> { case Ctr(_, Seq(s, x, b), p) => CSPOMConstraint(b)('member)(s, x) withParams p },

    'alldifferent_except_0 -> { case Ctr(_, Seq(IntExpression.simpleSeq(y)), p) =>
      allDifferent(y: _*) withParams (p + ("except" -> Seq(0)))
    },

    'diffn -> { case Ctr(_, Seq(IntExpression.simpleSeq(x), IntExpression.simpleSeq(y), IntExpression.simpleSeq(dx), IntExpression.simpleSeq(dy)), p) =>
      val origins = (x, y).zipped.map { (x, y) => CSPOMSeq(x, y) }
      val lengths = (dx, dy).zipped.map { (x, y) => CSPOMSeq(x, y) }

      CSPOMConstraint('diffn)(origins, lengths) withParams (p + ("zeroIgnored" -> false))
    }
  )
    .map {
      case (symbol, function) =>
        new GlobalCompiler {
          val functions: Functions = Functions(symbol)
          override val constraintMatcher: PartialFunction[CSPOMConstraint[_], Seq[CSPOMConstraint[_]]] =
            function.andThen(Seq(_))
        }
    }


  private def dfa(Q: Int, S: Int, fd: IndexedSeq[Int]): Map[(Int, Int), Int] = {
    var r = Map[(Int, Int), Int]()
    for (q <- 1 to Q; s <- 1 to S) {
      val t = fd((q - 1) * S + s - 1)
      if (t > 0) {
        r += (q, s) -> t
      }
    }
    r
  }

}
