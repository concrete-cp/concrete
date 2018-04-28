package concrete.constraint.semantic

import bitvectors.BitVector
import concrete._
import concrete.constraint._
import concrete.util.Interval

object Div {

  def splitRealDiv(i0: Interval, i1: Interval): (Option[Interval], Option[Interval]) = {
    val negative = i1.to(-1).flatMap(i0 / _)
    val positive = i1.from(1).flatMap(i0 / _)
    (negative, positive)
  }

  /**
    * Performs integer division (warning, operation / in Interval performs standard rounded division)
    */
  def intDiv(i0: Interval, i1: Interval): Interval = {
    val Interval(a, b) = i0
    val Interval(c, d) = i1

    val ac = a / c
    val ad = a / d
    val bc = b / c
    val bd = b / d

    val l = ac min ad min bc min bd

    val u = ac max ad max bc max bd

    Interval(l, u)
  }

  def reminder(xSpan: Interval, ySpan: Interval): Interval = {
    val ub = ySpan.abs.ub - 1
    if (xSpan.lb >= 0) {
      Interval(0, ub)
    } else if (xSpan.ub < 0) {
      Interval(-ub, 0)
    } else {
      Interval(-ub, ub)
    }
  }

  def remove0Bound(dom: Interval): Option[Interval] = dom.except(0).map {
    case Left(i) => i
    case Right((i1, i2)) => i1 span i2
  }
}

/**
  * @author vion
  *         x ÷ y = z (integer division)
  *
  *
  *
  *         Filtering is done on the property that x = y * (x ÷ y) + x % y
  *
  *         x ÷ y is computed by Div.div
  *         x % y is computed by Div.reminder
  */
class DivBC(x: Variable, y: Variable, z: Variable) extends Constraint(x, y, z) with BC with ItvArrayFixPoint {
  val ops = Array(reviseX(_), reviseY(_), reviseZ(_))

  def check(t: Array[Int]): Boolean = {
    t(1) != 0 && t(0) / t(1) == t(2)
  }

  def advise(ps: ProblemState, pos: Int) = 3

  def init(ps: ProblemState): Outcome = Div.remove0Bound(ps.span(y))
    .map(ps.shaveDom(y, _))
    .getOrElse(Contradiction(y))

  def revise(ps: ProblemState, mod: BitVector): Outcome = fixPoint(ps)

  def simpleEvaluation: Int = 1

  private def reviseZ(doms: Array[Domain]): Option[Interval] = {
    val x = doms(0).span
    val pos = doms(1).spanFrom(1).map(Div.intDiv(x, _))
    val neg = doms(1).spanTo(-1).map(Div.intDiv(x, _))
    // println(s"Two parts: $pos, $neg")
    Interval.unionSpan(neg, pos)
  }

  private def reviseY(doms: Array[Domain]): Option[Interval] = {
    // y = (x - x % y)/z
    val y = doms(1).span
    val x = doms(0).span

    val dividend = x - Div.reminder(x, y)
    // Z is split in three parts : < 0, > 0 and = 0
    // println(dividend)

    val pos = doms(2).spanFrom(1)
      .flatMap(dividend / _)
      .flatMap(y.intersect)

    val neg = doms(2).spanTo(-1)
      .flatMap(dividend / _)
      .flatMap(y.intersect)

    // For Z to be = 0, Y needs to be >/< |X|.lb
    val zero = if (doms(2).contains(0)) {
      val lb = x.abs.lb + 1
      val y0Pos = doms(1).spanFrom(lb).flatMap(_.intersect(y))
      val y0Neg = doms(1).spanTo(-lb).flatMap(_.intersect(y))
      //println(s"Four parts: $pos, $neg, $y0Pos, $y0Neg")
      Interval.unionSpan(y0Pos, y0Neg)
    } else {
      //println(s"Two parts: $pos, $neg")
      None
    }


    Interval.unionSpan(
      Interval.unionSpan(pos, neg),
      zero)
  }

  private def reviseX(doms: Array[Domain]): Option[Interval] = {
    // x = z * y + x % y
    val x = doms(0).span
    val y = doms(1).span
    val z = doms(2).span
    val r = Div.reminder(x, y)
    Some(z * y + r)
  }
}


/**
  * v0 / v1 = result
  *
  * @param v0
  * @param v1
  * @param result
  */
class DivAC(v0: Variable, v1: Variable, result: Variable, val skipIntervals: Boolean = true) extends Constraint(v0, v1, result) with Residues
  with TupleEnumerator
  with BCCompanion {

  def check(t: Array[Int]): Boolean = {
    t(1) != 0 && t(0) / t(1) == t(2)
  }

  override def advise(ps: ProblemState, pos: Int): Int = {
    val d0 = ps.card(result)
    val d1 = ps.card(v0)
    val d2 = ps.card(v1)
    val e = d0 * d1 + d0 * d2 + d1 * d2
    if (skip(ps, e)) -2 else e
  }

  override def findSupport(doms: Array[Domain], position: Int, value: Int): Option[Array[Int]] = {
    position match {
      case 0 => doms(1).find(y => y != 0 && doms(2).contains(value / y)).map(y => Array(value, y, value / y))
      case 1 => if (value == 0) None else doms(0).find(x => doms(2).contains(x / value)).map(x => Array(x, value, x / value))
      case 2 =>
        //println(s"Searching for a support of result = $value")

        for {
          b <- doms(1)
          r <- if (signum(value) == signum(b)) 0 until math.abs(b) else 0 until -math.abs(b) by -1
        } {
          val a = b * value + r
          if (doms(0).contains(a)) {
            //println(s"Found $a = $b * $value + $r")
            return Some(Array(a, b, value))
          }
        }
        None
      // super[TupleEnumerator].findSupport(doms, position, value)
    }
  }

  private def signum(x: Int): Int = if (x < 0) -1 else 1

}