package concrete.generator.cspompatterns

import concrete.CSPOMDriver
import concrete.generator.SumGenerator
import cspom.CSPOM._
import cspom.CSPOMConstraint
import cspom.compiler.ACCSE
import cspom.variable._

import scala.util.hashing.MurmurHash3

object SumSE extends ACCSE[IntPair] {

  def populate(c: CSPOMConstraint[_]): Iterator[IntPair] = {
    if (c.function == 'sum) {
      val (vars, coefs, _, _) = SumGenerator.readCSPOM(c)
      for (Seq((k1, v1), (k2, v2)) <- (coefs zip vars).combinations(2)) yield {
        IntPair(k1, v1, k2, v2)
      }
    } else {
      Iterator.empty
    }
  }

  def replace(pairexp: IntPair, ls: Seq[CSPOMConstraint[_]], dn: CSPOMExpression[_] => String): Seq[CSPOMConstraint[_]] = {

    val aux = IntVariable.free()
    val newConstraint = CSPOMDriver.linear(Seq(pairexp.v1, pairexp.v2, aux), Seq(pairexp.k1, pairexp.k2, -1), "eq", 0)

    logger.info(s"New subexpression in ${ls.size} constraints: ${aux.toString(dn)} = ${pairexp.k1}·${pairexp.v1.toString(dn)} + ${pairexp.k2}·${pairexp.v2.toString(dn)}")

    val newConstraints = for (a <- ls) yield {
      val (vars, coefs, constant, mode) = SumGenerator.readCSPOM(a)

      // Compute proportion wrt pairexp
      val indexV1 = vars.indexWhere(_ == pairexp.v1)
      val k = coefs(indexV1) / pairexp.k1

      // Integer division should be correct
      assert(pairexp.k1 * k == coefs(indexV1))

      // Remove V1 and C1
      val remV1 = vars.patch(indexV1, Nil, 1)
      val remC1 = coefs.patch(indexV1, Nil, 1)

      val indexV2 = remV1.indexWhere(_ == pairexp.v2)

      assert(remC1(indexV2) == pairexp.k2 * k)

      // Remove V2 and C2
      val remVars = remV1.patch(indexV2, Nil, 1)
      val remCoefs = remC1.patch(indexV2, Nil, 1)

      // Replace pairexp with aux variable
      CSPOMConstraint(a.result)('sum)(k +: remCoefs, seq2CSPOMSeq(aux +: remVars), CSPOMConstant(constant)) withParam ("mode" -> mode)
      // CSPOMDriver.linear(aux +: remVars, k +: remCoefs, mode.toString, constant)
    }

    newConstraint +: newConstraints
  }


}

trait IntPair {
  def k1: Int

  def k2: Int

  def v1: SimpleExpression[_]

  def v2: SimpleExpression[_]
}


class EqualPair(val v1: SimpleExpression[_], val v2: SimpleExpression[_]) extends IntPair {
  def k1 = 1

  def k2 = 1

  override def equals(o: Any): Boolean = o match {
    case p: EqualPair => (v1, v2) == ((p.v1, p.v2)) || (v1, v2) == ((p.v2, p.v1))
    case _ => false
  }

  override def hashCode(): Int = {
    MurmurHash3.unorderedHash(Seq(v1, v2))
  }
}

case class CoefPair(k1: Int, v1: SimpleExpression[_], k2: Int, v2: SimpleExpression[_]) extends IntPair

object IntPair {
  def apply(k1: Int, v1: SimpleExpression[_], k2: Int, v2: SimpleExpression[_]): IntPair = {
    if (k1 < k2) {
      val gcd = concrete.util.Math.gcd(Math.abs(k1), Math.abs(k2))
      CoefPair(k1 / gcd, v1, k2 / gcd, v2)
    } else if (k1 > k2) {
      val gcd = concrete.util.Math.gcd(Math.abs(k1), Math.abs(k2))
      CoefPair(k2 / gcd, v2, k1 / gcd, v1)
    } else {
      assert(concrete.util.Math.gcd(Math.abs(k1), Math.abs(k2)) == k1)
      new EqualPair(v1, v2)
    }
  }
}

object ClauseSE extends ACCSE[Clause] {
  def populate(c: CSPOMConstraint[_]): Iterator[Clause] = {
    if (c.function == 'clause) {
      val Seq(SimpleExpression.simpleSeq(positive), SimpleExpression.simpleSeq(negative)) = c.arguments
      for (Seq((k1, v1), (k2, v2)) <- (positive.map(e => (false, e)) ++ negative.map(e => (true, e))).combinations(2)) yield {
        Clause(k1, v1, k2, v2)
      }
    } else {
      Iterator.empty
    }
  }


  def replace(pair: Clause,
              ls: Seq[CSPOMConstraint[_]], dn: CSPOMExpression[_] => String): Seq[CSPOMConstraint[_]] = {
    val aux = new BoolVariable()
    val newConstraint = CSPOMConstraint(aux)('clause)(pair.pos.toSeq, pair.neg.toSeq)

    logger.info(s"New subexpression in ${ls.size} constraints: ${aux.toString(dn)} = ${pair.pos.map(_.toString(dn)).mkString("\\/")} \\/ ${pair.neg.map("-" + _.toString(dn)).mkString("\\/")}")

    val newConstraints = for (a <- ls) yield {
      val Seq(SimpleExpression.simpleSeq(positive), SimpleExpression.simpleSeq(negative)) = a.arguments

      CSPOMConstraint(a.result)('clause)(aux +: positive.filterNot(pair.pos.contains), negative.filterNot(pair.neg.contains))
    }

    newConstraint +: newConstraints
  }


}

case class Clause(pos: Set[SimpleExpression[Any]], neg: Set[SimpleExpression[Any]])

object Clause {
  def apply(neg1: Boolean, v1: SimpleExpression[_], neg2: Boolean, v2: SimpleExpression[_]): Clause = {
    var positive: Set[SimpleExpression[_]] = Set.empty
    var negative: Set[SimpleExpression[_]] = Set.empty
    if (neg1) {
      negative += v1
    } else {
      positive += v1
    }
    if (neg2) {
      negative += v2
    } else {
      positive += v2
    }
    Clause(positive, negative)
  }
}

object MinMaxSE extends ACCSE[MinMaxPair] {

  def populate(c: CSPOMConstraint[_]): Iterator[MinMaxPair] = {
    if (c.function == 'min || c.function == 'max) {
      for (Seq(v1, v2) <- c.arguments.combinations(2)) yield {
        MinMaxPair(min = c.function == 'min, Set(v1, v2))
      }
    } else {
      Iterator.empty
    }
  }

  def replace(pair: MinMaxPair, ls: Seq[CSPOMConstraint[_]], dn: CSPOMExpression[_] => String): Seq[CSPOMConstraint[_]] = {
    val function = if (pair.min) 'min else 'max
    val aux = IntVariable.free()
    val newConstraint = CSPOMConstraint(aux)(function)(pair.v.toSeq: _*)
    val newConstraints = for (a <- ls) yield {
      assert(a.function == function)
      CSPOMConstraint(a.result)(function)(aux +: a.arguments.filterNot(pair.v.contains):_*)
    }
    newConstraint +: newConstraints
  }


}


case class MinMaxPair(min: Boolean, v: Set[CSPOMExpression[Any]])