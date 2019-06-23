package concrete.constraint.semantic

import bitvectors.BitVector
import concrete.constraint._
import concrete.util.Interval
import concrete._

/*
  I have $x_a$, $x_b$, $y_a$, $y_b$, $z_a$ and $z_b$, all in $\mathbb Z$. They are used to define intervals $x = [x_a, x_b]$ and so on.

What I want is, given some relation, for each variable $x, y$ and $z$, to deduce its bounds (as tightly as possible) knowing the bounds of the two others (using only constant-time operations).

The relations are the classical integer operators $+, -, \times, \div$ (integer division) and $\%$ (modulo). Operators $+, -$ and $\times$ are reasonably handled by standard interval arithmetic, but integer division and modulo are trickier. Interval arithmetic handles reasonably the bounds of $z$ for $x \div y = z$ (with some caveats if $y$ contains 0), but I failed to find tight bounds for all other cases.

For $z$ in $x \% y = z$, I have only a list of very coarse filtering operations such as $z \subseteq [0, y_b - 1]$ if $x_a > 0$ and $y_a >0$, plus a bunch of other properties to handle negative $x$ and $y$. I think, however, that these bounds can be improved.

The central property I use that $x \% y = x - y \cdot (x \div y)$, but is is


.  It seems to work well, but I am open to counter-examples that would show that tighter bounds could be obtained.

For $x$, the interval is obviously infinite, so I am only interested in *tightening* known bounds $x_a$ and $x_b$ ($x'$ is the tightened interval). For this I currently use the same reformulation as before with $x' = y \cdot (x \div y) + z$.
  */

/**
  * @author vion
  *         x % y = z
  */
class ModBC(x: Variable, y: Variable, z: Variable) extends Constraint(x, y, z) with BC with ItvArrayFixPoint {
  val ops = Array(reviseX, reviseY, reviseZ)

  def init(ps: ProblemState): Outcome = Div.remove0Bound(ps.span(y))
    .map(ps.shaveDom(y, _))
    .getOrElse(Contradiction(y))

  def revise(ps: ProblemState, mod: BitVector): Outcome = {
    fixPoint(ps)
  }

  // Members declared in concrete.constraint.Constraint
  def advise(problemState: ProblemState, pos: Int): Int = 3

  def check(tuple: Array[Int]): Boolean = tuple(1) != 0 && tuple(0) % tuple(1) == tuple(2)

  def simpleEvaluation: Int = 1

  override def toString(ps: ProblemState): String = {
    s"${z.toString(ps)} =BC= ${x.toString(ps)} % ${y.toString(ps)}"
  }

  private def reviseZ(itv: Array[Domain]): Option[Interval] = {
    val x = itv(0).span

    def apply(y: Interval): Option[Interval] = {
      assert(!y.contains(0))
      // Ensures that z is in [0, y - 1] (positive case, negative cases are handled)
      val z1 = Div.reminder(x, y)

      /**
        * Let q = x ÷ y
        *
        * x = y * q + z <=> z = x - y * q
        */
      val q = Div.intDiv(x, y)
      val z2 = x - y * q
      z1 intersect z2
    }

    // y should be larger than z
    val lb = itv(2).span.abs.lb + 1

    val yPos = itv(1).spanFrom(lb).flatMap(apply)
    val yNeg = itv(1).spanTo(-lb).flatMap(apply)

    Interval.unionSpan(yPos, yNeg)
  }

  private def reviseY(itv: Array[Domain]): Option[Interval] = {
    val x = itv(0).span
    val z = itv(2).span

    // y = (x - z) / (x ÷ y)
    def apply(y: Option[Interval]) = {
      y.map(Div.intDiv(x, _))
        .flatMap { q =>
          if (q.contains(0)) {
            // No filtering :(
            y
          } else {
            (x - z) / q
          }
        }
    }


    // y should be larger than z
    val lb = z.abs.lb + 1

    Interval.unionSpan(
      apply(itv(1).spanFrom(lb)),
      apply(itv(1).spanTo(-lb)))

  }


  private def reviseX(itv: Array[Domain]): Option[Interval] = {

    val z = itv(2).span


    // X is of the same sign as Z
    val xo = if (z.lb > 0) {
      itv(0).spanFrom(0)
    } else if (z.ub < 0) {
      itv(0).spanTo(0)
    } else {
      Some(itv(0).span)
    }

    // y should be larger than z
    val lb = z.abs.lb + 1


    xo.flatMap {
      x =>
        val pos = for {
          y <- itv(1).spanFrom(lb)
        } yield {
          z + Div.intDiv(x, y) * y
        }

        val neg = for {
          y <- itv(1).spanTo(-lb)
        } yield {
          z + Div.intDiv(x, y) * y
        }

        Interval.unionSpan(pos, neg)
    }
  }


}

class ModAC(v0: Variable, v1: Variable, result: Variable) extends Constraint(v0, v1, result) with Residues with TupleEnumerator {
  def check(t: Array[Int]): Boolean = t(0) % t(1) == t(2)

  override def findSupport(doms: Array[Domain], position: Int, value: Int): Option[Array[Int]] =
    super[TupleEnumerator].findSupport(doms, position, value)

  override def toString(ps: ProblemState): String = {
    s"${result.toString(ps)} =AC= ${v0.toString(ps)} % ${v1.toString(ps)}"
  }
}