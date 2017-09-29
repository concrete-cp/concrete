package concrete.generator.cspompatterns

import concrete.CSPOMDriver._
import cspom.CSPOM._
import cspom.compiler.{Ctr, GlobalCompiler}
import cspom.extension.MDDRelation
import cspom.variable._
import cspom.{CSPOM, CSPOMConstraint}

object FZPatterns {
  val mtch: PartialFunction[CSPOMConstraint[_], CSPOMConstraint[_]] = {
    /*
     *  (∀ i ∈ 1..n : as[i]) ↔ r where n is the length of as
     * array_bool_and(array [int] of var bool: as, var bool: r)
     */
    case Ctr('array_bool_and, Seq(CSPOMSeq(as), r), p) =>
      CSPOMConstraint(r, 'and, as, p)

    /*
     *  b ∈ 1..n ∧ as[b] = c where n is the length of as
     * array_bool_element(var int: b, array [int] of bool: as, var bool: c)
     */
    case Ctr('array_bool_element, Seq(b, as, c), p) =>
      CSPOMConstraint(c, 'element, Seq(as, b), p)

    /*
     * (∃ i ∈ 1..n : as[i]) ↔ r where n is the length of as
     * array_bool_or(array [int] of var bool: as, var bool: r)
     */
    case Ctr('array_bool_or, Seq(as, r), p) =>
      CSPOMConstraint(r, 'clause, Seq(as, CSPOMSeq.empty), p)

    /*
     * (((i ∈ 1..n : as[i]) mod 2) = 1) where n is the length of as
     * array_bool_xor(array [int] of var bool: as)
     */
    case Ctr('array_bool_xor, Seq(CSPOMSeq(as)), p) =>
      CSPOMConstraint('xor)(as: _*) withParams p

    /*
     * b ∈ 1..n ∧ as[b] = c where n is the length of as
     * array_float_element(var int: b, array [int] of float: as, var float: c)
     */
    /*
     * b ∈ 1..n ∧ as[b] = c where n is the length of as
     * array_int_element(var int: b, array [int] of int: as, var int: c)
     */
    case Ctr('array_int_element, Seq(b, as, c), p) => {
      CSPOMConstraint(c, 'element, Seq(as, b), p)
    }
    /*
     * b ∈ 1..n ∧ as[b] = c where n is the length of as
     * array_set_element(var int: b, array [int] of set of int: as, set of int: c)
     */
    /*
     * b ∈ 1..n ∧ as[b] = c where n is the length of as
     * array_var_bool_element(var int: b, array [int] of var bool: as, var bool: c)
     */
    case Ctr('array_var_bool_element, Seq(b, as, c), p) => {
      CSPOMConstraint(c, 'element, Seq(as, b), p)
    }
    /*
     * b ∈ 1..n ∧ as[b] = c where n is the length of as
     * array_var_float_element(var int: b, array [int] of var float: as, var float: c)
     */
    /*
     * b ∈ 1..n ∧ as[b] = c where n is the length of as
     * array_var_int_element(var int: b, array [int] of var int: as, var int: c)
     */
    case Ctr('array_var_int_element, args, p) => {
      val Seq(b, as, c) = args
      CSPOMConstraint(c, 'element, Seq(as, b), p)
    }
    /*
     * b ∈ 1..n ∧ as[b] = c where n is the length of as
     * array_var_set_element(var int: b, array [int] of var set of int: as, var set of int: c)
     */
    /*
     * (a ↔ b = 1) ∧ (¬a ↔ b = 0)
     * bool2int(var bool: a, var int: b)
     */
    case Ctr('bool2int, args, p) => CSPOMConstraint('eq)(args: _*) withParams p
    /*
     * (a ∧ b) ↔ r
     * bool_and(var bool: a, var bool: b, var bool: r)
     */
    case Ctr('bool_and, Seq(a, b, r), p) =>
      CSPOMConstraint(r, 'and, Seq(a, b), p)
    /*
     * (∃ i ∈ 1..nas : as[i]) ∨ (∃ i ∈ 1..nbs : ¬bs[i]) where n is the length of as
     * bool_clause(array [int] of var bool: as, array [int] of var bool: bs)
     */
    case Ctr('bool_clause, Seq(BoolExpression.seq(as), BoolExpression.seq(bs)), p) =>
      clause(as, bs) withParams (p)

    /*
     * a = b
     * bool_eq(var bool: a, var bool: b)
     */
    case Ctr('bool_eq, a: Seq[_], p) =>
      CSPOMConstraint('eq)(a: _*) withParams p
    /*
     * (a = b) ↔ r
     * bool_eq_reif(var bool: a, var bool: b, var bool: r)
     */
    case Ctr('bool_eq_reif, Seq(a, b, r), p) =>
      new CSPOMConstraint(r, 'eq, Seq(a, b), p)
    /*
     * ¬a ∨ b
     * bool_le(var bool: a, var bool: b)
     */
    case Ctr('bool_le, Seq(BoolExpression(a), BoolExpression(b)), p) =>
      clause(b)(a) withParams p
    /*
     * (¬a ∨ b) ↔ r
     * bool_le_reif(var bool: a, var bool: b, var bool: r)
     */
    case Ctr('bool_le_reif, Seq(a, b, r), p) =>
      new CSPOMConstraint(r, 'clause, Seq(Seq(b), Seq(a)), p)

    /*
     * (¬a /\ b) ↔ r
     * bool_lt_reif(var bool: a, var bool: b, var bool: r)
     */
    case Ctr('bool_lt_reif, Seq(a, b, r), p) =>
      CSPOMConstraint(r)('sum)(Seq(1, -1), Seq(a, b), CSPOMConstant(0)) withParams p + ("mode" -> "lt")
    /*
     * i ∈ 1..n : as[i].bs[i] = c where n is the common length of as and bs
     * bool_lin_eq(array [int] of int: as, array [int] of var bool: bs, var int: c)
     */
    case Ctr('bool_lin_eq, Seq(IntExpression.constSeq(as), BoolExpression.simpleSeq(bs), CSPOMConstant(c: Int)), p) =>
      pseudoBoolean(bs, as, "eq", c) withParams p
    /*
     * i ∈ 1..n : as[i].bs[i] ≤ c where n is the common length of as and bs
     * bool_lin_le(array [int] of int: as, array [int] of var bool: bs, int: c)
     */
    case Ctr('bool_lin_le, Seq(IntExpression.constSeq(as), BoolExpression.simpleSeq(bs), CSPOMConstant(c: Int)), p) =>
      pseudoBoolean(bs, as, "le", c) withParams p
    /*
     * ¬a ∧ b
     * bool_lt(var bool: a, var bool: b)
     */
    /*
     * (¬a ∧ b) ↔ r
     * bool_lt_reif(var bool: a, var bool: b, var bool: r)
     */
    /*
     * ¬a = b
     * bool_not(var bool: a, var bool: b)
     */
    /*
     * (a ∨ b) ↔ r
     * bool_or(var bool: a, var bool: b, var bool: r)
     */
    /*
     * (a = b) ↔ r
     * bool_xor(var bool: a, var bool: b, var bool: r)
     */
    case Ctr('bool_xor, Seq(a, b, r), p) =>
      CSPOMConstraint(r, 'xor, Seq(a, b), p)

    /*
     * count_eq(array[int] of var int: x, var int: y, var int: c)
     *
     */
    case Ctr('count_eq, Seq(x, y, c), p) =>
      CSPOMConstraint(c, 'occurrence, Seq(y, x), p)

    case Ctr('count_le, Seq(x, y, c), p) =>
      CSPOMConstraint('atMost)(c, y, x) withParams p

    /*
     * |a| = b
     * float_abs(var float: a, var float: b)
     */
    /*
     * acos a = b
     * float_acos(var float: a, var float: b)
     */
    /*
     * asin a = b
     * float_asin(var float: a, var float: b)
     */
    /*
     * atan a = b
     * float_atan(var float: a, var float: b)
     */
    /*
     * cos a = b
     * float_cos(var float: a, var float: b)
     */
    /*
     * cosh a = b
     * float_cosh(var float: a, var float: b)
     */
    /*
     * exp a = b
     * float_exp(var float: a, var float: b)
     */
    /*
     * ln a = b
     * float_ln(var float: a, var float: b)
     */
    /*
     * log 10 a = b
     * float_log10(var float: a, var float: b)
     */
    /*
     * log 2 a = b
     * float_log2(var float: a, var float: b)
     */
    /*
     * √a = b
     * float_sqrt(var float: a, var float: b)
     */
    /*
     * sin a = b
     * float_sin(var float: a, var float: b)
     */
    /*
     * sinh a = b
     * float_sinh(var float: a, var float: b)
     */
    /*
     * tan a = b
     * float_tan(var float: a, var float: b)
     */
    /*
     * tanh a = b
     * float_tanh(var float: a, var float: b)
     */
    /*
     * a = b
     * float_eq(var float: a, var float: b)
     */
    /*
     * (a = b) ↔ r
     * float_eq_reif(var float: a, var float: b, var bool: r)
     */
    /*
     * a ≤ b
     * float_le(var float: a, var float: b)
     */
    /*
     * (a ≤ b) ↔ r
     * float_le_reif(var float: a, var float: b, var bool: r)
     */
    /*
     * i ∈ 1..n : as[i].bs[i] = c where n is the common length of as and bs
     * float_lin_eq(array [int] of float: as, array [int] of var float: bs, float: c)
     */
    /*
     * (i ∈ 1..n : as[i].bs[i] = c) ↔ r where n is the common length of as and bs
     * float_lin_eq_reif(array [int] of float: as, array [int] of var float: bs, float: c, var bool: r)
     */
    /*
     * i ∈ 1..n : as[i].bs[i] ≤ c where n is the common length of as and bs
     * float_lin_le(array [int] of float: as, array [int] of var float: bs, float: c)
     */
    /*
     * (i ∈ 1..n : as[i].bs[i] ≤ c) ↔ r where n is the common length of as and bs
     * float_lin_le_reif(array [int] of float: as, array [int] of var float: bs, float: c, var bool: r)
     */
    /*
     * i ∈ 1..n : as[i].bs[i] < c where n is the common length of as and bs
     * float_lin_lt(array [int] of float: as, array [int] of var float: bs, float: c)
     */
    /*
     * (i ∈ 1..n : as[i].bs[i] < c) ↔ r where n is the common length of as and bs
     * float_lin_lt_reif(array [int] of float: as, array [int] of var float: bs, float: c, var bool: r)
     */
    /*
     * i ∈ 1..n : as[i].bs[i] = c where n is the common length of as and bs
     * float_lin_ne(array [int] of float: as, array [int] of var float: bs, float: c)
     */
    /*
     * (i ∈ 1..n : as[i].bs[i] = c) ↔ r where n is the common length of as and bs
     * float_lin_ne_reif(array [int] of float: as, array [int] of var float: bs, float: c, var bool: r)
     */
    /*
     * a < b
     * float_lt(var float: a, var float: b)
     */
    /*
     * (a < b) ↔ r
     * float_lt_reif(var float: a, var float: b, var bool: r)
     */
    /*
     * max(a, b) = c
     * float_max(var float: a, var float: b, var float: c)
     */
    /*
     * min(a, b) = c
     * float_min(var float: a, var float: b, var float: c)
     */
    /*
     * a = b
     * float_ne(var float: a, var float: b)
     */
    /*
     * (a = b) ↔ r
     * float_ne_reif(var float: a, var float: b, var bool: r)
     */
    /*
     * a+b = c
     * float_plus(var float: a, var float: b, var float: c)
     */
    /*
     * |a| = b
     * int_abs(var int: a, var int: b)
     */
    case Ctr('int_abs, Seq(a, b), p) => CSPOMConstraint(b, 'abs, Seq(a), p)
    /*
     * a/b = c rounding towards zero.
     * int_div(var int: a, var int: b, var int: c)
     */
    case Ctr('int_div, Seq(a, b, c), p) => CSPOMConstraint(c, 'div, Seq(a, b), p)
    /*
     * a = b
     * int_eq(var int: a, var int: b)
     */
    case Ctr('int_eq, args, p) => CSPOMConstraint('eq)(args: _*) withParams p
    /*
     * (a = b) ↔ r
     * int_eq_reif(var int: a, var int: b, var bool: r)
     */
    case Ctr('int_eq_reif, Seq(a, b, r), p) =>
      CSPOMConstraint(r, 'eq, Seq(a, b), p)
    /*
     * a ≤ b
     * int_le(var int: a, var int: b)
     */
    case Ctr('int_le, Seq(IntExpression(a), IntExpression(b)), p) =>
      1 *: a + -1 *: b <= 0 withParams p
    /*
     * (a ≤ b) ↔ r
     * int_le_reif(var int: a, var int: b, var bool: r)
     */
    case Ctr('int_le_reif, Seq(a, b, r), p) =>
      CSPOMConstraint(r)('sum)(Seq(1, -1), Seq(a, b), CSPOMConstant(0)) withParams (p + ("mode" -> "le"))

    /*
     * i ∈ 1..n : as[i].bs[i] = c where n is the common length of as and bs
     * int_lin_eq(array [int] of int: as, array [int] of var int: bs, int: c)
     */
    case Ctr('int_lin_eq, Seq(IntExpression.constSeq(as), IntExpression.simpleSeq(bs), CSPOMConstant(c: Int)), p) =>
      linear(bs, as, "eq", c) withParams (p)

    /*
     * i ∈ 1..n : as[i].bs[i] = c) ↔ r where n is the common length of as and bs
     * int_lin_eq_reif(array [int] of int: as, array [int] of var int: bs, int: c, var bool: r)
     */
    case Ctr('int_lin_eq_reif, Seq(as, bs, c, r), p) =>
      CSPOMConstraint(r)('sum)(as, bs, c) withParams (p + ("mode" -> "eq"))

    /*
     * i ∈ 1..n : as[i].bs[i] ≤ c where n is the common length of as and bs
     * int_lin_le(array [int] of int: as, array [int] of var int: bs, int: c)
     */
    case Ctr('int_lin_le, Seq(IntExpression.constSeq(as), IntExpression.simpleSeq(bs), CSPOMConstant(c: Int)), p) =>
      linear(bs, as, "le", c) withParams p

    /*
     * (i ∈ 1..n : as[i].bs[i] ≤ c) ↔ r where n is the common length of as and bs
     * int_lin_le_reif(array [int] of int: as, array [int] of var int: bs, int: c, var bool: r)
     */
    case Ctr('int_lin_le_reif, Seq(as, bs, c, r), p) =>
      CSPOMConstraint(r)('sum)(as, bs, c) withParams p + ("mode" -> "le")

    /*
     * i ∈ 1..n : as[i].bs[i] = c where n is the common length of as and bs
     * int_lin_ne(array [int] of int: as, array [int] of var int: bs, int: c)
     */
    case Ctr('int_lin_ne, Seq(IntExpression.constSeq(as), IntExpression.simpleSeq(bs), CSPOMConstant(c: Int)), p) =>
      linear(bs, as, "ne", c) withParams p
    /*
     * (i ∈ 1..n : as[i].bs[i] = c) ↔ r where n is the common length of as and bs
     * int_lin_ne_reif(array [int] of int: as, array [int] of var int: bs, int: c, var bool: r)
     */
    case Ctr('int_lin_ne_reif, Seq(as, bs, c, r), p) =>
      CSPOMConstraint(r)('sum)(as, bs, c) withParams p + ("mode" -> "ne")
    /*
     * a < b
     * int_lt(var int: a, var int: b)
     */
    case Ctr('int_lt, Seq(IntExpression(a), IntExpression(b)), p) =>
      1 *: a + -1 *: b < 0 withParams p
    /*
     * (a < b) ↔ r
     * int_lt_reif(var int: a, var int: b, var bool: r)
     */
    case Ctr('int_lt_reif, Seq(a, b, r), p) =>
      CSPOMConstraint(r)('sum)(Seq(1, -1), Seq(a, b), CSPOMConstant(0)) withParams p + ("mode" -> "lt")
    /*
     * max(a, b) = c
     * int_max(var int: a, var int: b, var int: c)
     */
    case Ctr('int_max, Seq(a: SimpleExpression[_], b: SimpleExpression[_], c: SimpleExpression[_]), p) =>
      CSPOMConstraint(c, 'max, Seq(a, b), p)
    /*
     * min(a, b) = c
     * int_min(var int: a, var int: b, var int: c)
     */
    case Ctr('int_min, Seq(a: SimpleExpression[_], b: SimpleExpression[_], c: SimpleExpression[_]), p) =>
      CSPOMConstraint(c, 'min, Seq(a, b), p)

    /**
      * predicate array_int_minimum(var int: m, array[int] of var int: x)
      */
    case Ctr('array_int_minimum, Seq(m, x: CSPOMSeq[_]), p) =>
      CSPOMConstraint(m, 'min, x.values, p)

    /**
      * predicate array_int_maximum(var int: m, array[int] of var int: x)
      */
    case Ctr('array_int_maximum, Seq(m, x: CSPOMSeq[_]), p) =>
      CSPOMConstraint(m, 'max, x.values, p)
    /*
     * a − x.b = c where x = a/b rounding towards zero.
     * int_mod(var int: a, var int: b, var int: c)
     */
    /*
     * a = b
     * int_ne(var int: a, var int: b)
     */
    case Ctr('int_ne, Seq(a, b), p) => CSPOMConstraint(CSPOMConstant(false), 'eq, Seq(a, b), p)
    /*
     * (a = b) ↔ r
     * int_ne_reif(var int: a, var int: b, var bool: r)
     */
    case Ctr('int_ne_reif, Seq(a, b, r), p) => CSPOMConstraint(r, 'ne, Seq(a, b), p)
    /*
     * a+b = c
     * int_plus(var int: a, var int: b, var int: c)
     */
    case Ctr('int_plus, Seq(IntExpression(a), IntExpression(b), IntExpression(c)), p) =>
      1 *: a + 1 *: b + -1 *: c === 0 withParams p

    /*
     * a×b = c
     * int_times(var int: a, var int: b, var int: c)
     */
    case Ctr('int_times, Seq(a, b, c), p) =>
      new CSPOMConstraint(c, 'mul, Seq(a, b), p)
    /*
     * a = b
     * int2float(var int: a, var float: b)
     */
    /*
     * |a| = b
     * set_card(var set of int: a, var int: b)
     */
    /*
     * a−b = c
     * set_diff(var set of int: a, var set of int: b, var set of int: c)
     */
    /*
     * a = b
     * set_eq(var set of int: a, var set of int: b)
     */
    /*
     * (a = b) ↔ r
     * set_eq_reif(var set of int: a, var set of int: b, var bool: r)
     */
    /*
     * a∈b
     * set_in(var int: a, var set of int: b)
     */

    /*
     * (a ∈ b) ↔ r
     * set_in_reif(var int: a, var set of int: b, var bool: r)
     */
    case Ctr('set_in_reif, Seq(a, CSPOMConstant(b: Seq[_]), r), p) => {
      new CSPOMConstraint(r, 'in, Seq(a, b.map { case i: Int => CSPOMConstant(i) }), p)
    }
    /*
     * a∩b = c
     * set_intersect(var set of int: a, var set of int: b, var set of int: c)
     */
    /*
     * a ⊆ b ∨ min(a b) ∈ a
     * set_le(var set of int: a, var set of int: b)
     */
    /*
     * a ⊂ b ∨ min(a b) ∈ a
     * set_lt(var set of int: a, var set of int: b)
     */
    /*
     * a = b
     * set_ne(var set of int: a, var set of int: b)
     */
    /*
     * (a = b) ↔ r
     * set_ne_reif(var set of int: a, var set of int: b, var bool: r)
     */
    /*
     * a⊆b
     * set_subset(var set of int: a, var set of int: b)
     */
    /*
     * (a ⊆ b) ↔ r
     * set_subset_reif(var set of int: a, var set of int: b, var bool: r)
     */
    /*
     * a b = c
     * set_symdiff(var set of int: a, var set of int: b, var set of int: c)
     */
    /*
     * a∪b = c
     * set_union(var set of int: a, var set of int: b, var set of int: c)
     */

    /*
     * FlatZinc flattens t.
     * predicate table_int(array[int] of var int: x, array[int, int] of int: t)
     */
    case Ctr('table_int, Seq(IntExpression.simpleSeq(x), IntExpression.constSeq(t)), p) =>
      x in MDDRelation(t.grouped(x.size).map(_.toArray).toSeq)

    /*
     *  predicate all_different_int(array[int] of var int: x);
     */
    case Ctr('all_different_int, Seq(IntExpression.simpleSeq(y)), p) =>
      allDifferent(y: _*) withParams p

    case Ctr('regular, Seq(x,
    CSPOMConstant(q: Int),
    CSPOMConstant(s: Int),
    IntExpression.constSeq(fd), //: CSPOMSeq[_],
    q0,
    f), p) =>
      val CSPOMConstant(fseq: Seq[Int]) = f
      CSPOMConstraint('regular)(x, q0, CSPOM.constantSeq(fseq)) withParams p + ("dfa" -> dfa(q, s, fd.toIndexedSeq))

    case Ctr('lex_lesseq_bool, Seq(x, y), p) =>
      CSPOMConstraint('lexleq)(x, y) withParams p

    case Ctr('member_int, Seq(s, x), p) =>
      CSPOMConstraint('member)(s, x) withParams p

    case Ctr('member_int_reif, Seq(s, x, b), p) =>
      CSPOMConstraint(b)('member)(s, x) withParams p

    case Ctr('alldifferent_except_0, Seq(IntExpression.simpleSeq(y)), p) =>
      allDifferent(y: _*) withParams (p + ("except" -> Seq(0)))
  }

  def apply() = Seq(
    new GlobalCompiler(mtch) {
      def selfPropagation = false
    })

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
