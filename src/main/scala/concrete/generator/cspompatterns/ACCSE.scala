package concrete.generator.cspompatterns

import com.typesafe.scalalogging.LazyLogging
import concrete.CSPOMDriver
import concrete.generator.SumGenerator
import cspom.CSPOM._
import cspom.CSPOMConstraint
import cspom.compiler.ACCSE
import cspom.variable._

object SumSE extends ACCSE[IntPair, Int] {
  override def constraintToArgs(c: CSPOMConstraint[_]): IndexedSeq[Arg] = {
    val (vars, coefs, _, _) = SumGenerator.readCSPOM(c)
    vars zip coefs
  }

  def filter(c: CSPOMConstraint[_]): Boolean = c.function == 'sum

  override def define(pairexp: IntPair, aux: CSPOMVariable[_]): (Arg, CSPOMConstraint[_]) = {
    val definition = CSPOMConstraint('sum)(
      Seq(CSPOMConstant(pairexp.k1), CSPOMConstant(pairexp.k2), CSPOMConstant(-1)),
      Seq(pairexp.v1, pairexp.v2, aux),
      CSPOMConstant(0))
      .withParam("mode" -> "eq")

    val arg = (aux, 1)

    (arg, definition)
  }

  override def toString(pairexp: IntPair, dn: CSPOMExpression[_] => String): String =
    s"${pairexp.k1}·${pairexp.v1.toString(dn)} + ${pairexp.k2}·${pairexp.v2.toString(dn)}"

  override def toString(pairexp: Arg, dn: CSPOMExpression[_] => String): String =
    s"${pairexp._2}·${pairexp._1.toString(dn)}"

  def replace(pairexp: IntPair, arg: Arg, constraint: Args): Boolean = {

    // Obtain actual coefficients, use options because some variables might already have been removed
    val r = for {
      k1 <- constraint.get(pairexp.v1)
      k2 <- constraint.get(pairexp.v2)
      k = k1 / pairexp.k1
      // Check also correction for second coef. May be wrong if there are several occurrences of the same variable in the scope!
      if k2 == pairexp.k2 * k
    } yield {
      // Integer division should be correct
      assert(pairexp.k1 * k == k1)

      // remove subexpression
      constraint -= pairexp.v1
      constraint -= pairexp.v2

      // Replace pairexp with aux variable
      constraint += arg.copy(_2 = k)
      //
    }
    r.isDefined
  }

  def pair(a1: Arg, a2: Arg): IntPair = {

    val (v1, k1) = a1
    val (v2, k2) = a2

    val gcd = IntPair.gcd(math.abs(k1), math.abs(k2))

    if (k1 < k2) {
      CoefPair(k1 / gcd, v1, k2 / gcd, v2)
    } else if (k1 > k2) {
      CoefPair(k2 / gcd, v2, k1 / gcd, v1)
    } else {
      assert(gcd == math.abs(k1), s"Coefficients $k1 and $k2 are equal, should also be equal to gcd $gcd")
      if (v1.hashCode < v2.hashCode) {
        EqualPair(v1, v2)
      } else {
        EqualPair(v2, v1)
      }
    }
  }

  override def argsToConstraint(original: CSPOMConstraint[_], args: Args): CSPOMConstraint[_] = {
    val (_, _, constant, mode) = SumGenerator.readCSPOM(original)
    val (vars, coefs) = args.unzip
    CSPOMConstraint(original.result)('sum)(seq2CSPOMSeq(coefs.map(CSPOMConstant(_))), seq2CSPOMSeq(vars), CSPOMConstant(constant)) withParam ("mode" -> mode)
  }
}

object IntPair {
  def even(a: Int): Boolean = (a & 0x1) == 0

  def gcd(ia: Int, ib: Int): Int = {
    var d = 0
    var a = ia
    var b = ib
    while (even(a) && even(b)) {
      a /= 2
      b /= 2
      d += 1
    }
    while (a != b) {
      if (even(a)) {
        a /= 2
      } else if (even(b)) {
        b /= 2
      } else if (a > b) {
        a = (a - b) / 2
      } else {
        b = (b - a) / 2
      }
    }

    a * (0x1 << d)
  }
}

sealed trait IntPair {
  def k1: Int

  def k2: Int

  def v1: CSPOMExpression[_]

  def v2: CSPOMExpression[_]
}


case class EqualPair(v1: CSPOMExpression[_], v2: CSPOMExpression[_]) extends IntPair {
  def k1 = 1

  def k2 = 1
}

case class CoefPair(k1: Int, v1: CSPOMExpression[_], k2: Int, v2: CSPOMExpression[_]) extends IntPair

case class Clause(pos: Set[CSPOMExpression[Any]], neg: Set[CSPOMExpression[Any]])

case class MinMaxPair(typ: MinMaxType, v: Set[CSPOMExpression[Any]])


object ClauseSE extends ACCSE[Clause, Boolean] {
//  def populate(c: CSPOMConstraint[_]): Iterator[Clause] = {
//    if (c.function == 'clause) {
//      val Seq(SimpleExpression.simpleSeq(positive), SimpleExpression.simpleSeq(negative)) = c.arguments
//      for (Seq((k1, v1), (k2, v2)) <- (positive.map(e => (false, e)) ++ negative.map(e => (true, e))).combinations(2)) yield {
//        Clause(k1, v1, k2, v2)
//      }
//    } else {
//      Iterator.empty
//    }
//  }
//
//
//  def replace(pair: Clause,
//              ls: Seq[CSPOMConstraint[_]], dn: CSPOMExpression[_] => String): Seq[CSPOMConstraint[_]] = {
//    val aux = new BoolVariable()
//    val newConstraint = CSPOMConstraint(aux)('clause)(pair.pos.toSeq, pair.neg.toSeq)
//
//    logger.info(s"New subexpression in ${ls.size} constraints: ${aux.toString(dn)} = ${(pair.pos.map(_.toString(dn)) ++ pair.neg.map("-" + _.toString(dn))).mkString(" ∨ ")}")
//
//    val newConstraints = for (a <- ls) yield {
//      val Seq(SimpleExpression.simpleSeq(positive), SimpleExpression.simpleSeq(negative)) = a.arguments
//
//      CSPOMConstraint(a.result)('clause)(aux +: positive.filterNot(pair.pos.contains), negative.filterNot(pair.neg.contains))
//    }
//
//    newConstraint +: newConstraints
//  }

  override def filter(c: CSPOMConstraint[_]): Boolean = c.function == 'clause

  override def pair(a1: Arg, a2: Arg): Clause = {
    var positive: Set[CSPOMExpression[_]] = Set.empty
    var negative: Set[CSPOMExpression[_]] = Set.empty
    for ((x, neg) <- Seq(a1, a2)) {
      if (neg) {
        negative += x
      } else {
        positive += x
      }
    }
    Clause(positive, negative)
  }

  override def define(pair: Clause, aux: CSPOMVariable[_]): (Arg, CSPOMConstraint[_]) = {
    val constraint = CSPOMConstraint(aux)('clause)(seq2CSPOMSeq(pair.pos), seq2CSPOMSeq(pair.neg))
    val arg = (aux, false)
    (arg, constraint)
  }

  override def replace(pair: Clause, arg: Arg, constraint: Args): Boolean = {
    if (pair.pos.forall(constraint.contains) && pair.neg.forall(constraint.contains)) {
      constraint --= pair.pos
      constraint --= pair.neg
      constraint += arg
      true
    } else {
      false
    }
  }

  override def constraintToArgs(c: CSPOMConstraint[_]): IndexedSeq[Arg] = {
    val Seq(SimpleExpression.simpleSeq(positive), SimpleExpression.simpleSeq(negative)) = c.arguments
    positive.map((_, false)) ++ negative.map((_, true))
  }

  override def argsToConstraint(original: CSPOMConstraint[_], args: Args): CSPOMConstraint[_] = {
    val (negative, positive) = args.partition(_._2)
    CSPOMConstraint(original.result)('clause)(seq2CSPOMSeq(positive.keys), seq2CSPOMSeq(negative.keys))
  }
}

sealed trait MinMaxType {
  def function: Symbol
}

case object MinType extends MinMaxType {
  def function = 'min
}

case object MaxType extends MinMaxType {
  def function = 'max
}

object MinMaxSE extends ACCSE[MinMaxPair, MinMaxType] {

  override def filter(c: CSPOMConstraint[_]): Boolean = c.function == 'min || c.function == 'max

  override def pair(a1: Arg, a2: Arg): MinMaxPair = {
    require(a1._2 == a2._2)
    MinMaxPair(a1._2, Set(a1._1, a2._1))
  }

  override def define(pair: MinMaxPair, aux: CSPOMVariable[_]): (Arg, CSPOMConstraint[_]) = {
    val constraint = CSPOMConstraint(aux)(pair.typ.function)(pair.v.toSeq: _*)
    val arg = (aux, pair.typ)
    (arg, constraint)
  }

  override def replace(pair: MinMaxPair, arg: Arg, constraint: Args): Boolean = {
    if (pair.v.forall(constraint.contains)) {
      constraint --= pair.v
      constraint += arg
      true
    } else {
      false
    }
  }

  override def constraintToArgs(c: CSPOMConstraint[_]): IndexedSeq[Arg] = {
    c.function match {
      case 'min => c.arguments.map(v => (v, MinType)).toIndexedSeq
      case 'max => c.arguments.map(v => (v, MaxType)).toIndexedSeq
    }
  }

  override def argsToConstraint(original: CSPOMConstraint[_], args: MinMaxSE.Args): CSPOMConstraint[_] = {
    val typ = args.head._2
    require(args.forall(_._2 == typ))
    CSPOMConstraint(original.result)(typ.function)(args.keys: _*)
  }
}