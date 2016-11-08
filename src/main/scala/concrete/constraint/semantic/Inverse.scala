package concrete.constraint.semantic

import concrete.Variable
import concrete.constraint.Constraint
import concrete.ProblemState
import concrete.Outcome
import concrete.constraint.StatefulConstraint
import cspom.util.BitVector
import concrete.constraint.Removals

case class Boxed[A](bv: A)

/**
 *  Constraint x(i) = j => y(j) = i
 *
 *  Note the one-sided implication. Post this constraint on both (x, y) and (y, x) to ensure equivalence.
 *
 *  y(j) != i => x(i) != j is enforced
 */
final class Inverse(x: Array[Variable], y: Array[Variable], xOffset: Int, yOffset: Int) extends Constraint(x ++ y)
    with StatefulConstraint[Boxed[BitVector]] with Removals {

  //  {
  //    val groups = scope.groupBy(identity).filter(_._2.size > 1)
  //    assert(groups.isEmpty, groups.toSeq)
  //  }

  def getEvaluation(problemState: ProblemState): Int = {
    val fx = problemState(this)
    val card = fx.bv.cardinality
    card * card
  }

  def check(tuple: Array[Int]): Boolean = {
    (0 until x.length).forall { i =>
      val j = tuple(i)
      tuple(j + x.length - yOffset) == i + xOffset
    }
  }

  def init(ps: ProblemState): Outcome = ps.updateState(this, Boxed(BitVector.filled(x.length)))

  def revise(problemState: ProblemState, mod: BitVector): Outcome = {
    //println(s"$mod of ${toString(problemState)}")
    // fx contains "free variables"
    var fx = problemState(this).bv

    // Check whether variables have been assigned
    val r = problemState.fold(mod.traversable.filter(_ < x.length)) { (ps, xPos) =>
      val dom = ps.dom(x(xPos))
      if (dom.isAssigned) {
        fx -= xPos

        val xVal = dom.singleValue

        val yPos = xVal - yOffset

        val yVal = xPos + xOffset

        ps.tryAssign(y(yPos), yVal)
      } else {
        ps
      }
    }
      .fold(fx.traversable) { (ps, xPos) =>
        ps.filterDom(x(xPos))(v => ps.dom(y(v - yOffset)).present(xPos + xOffset))
      }
      .updateState(this, Boxed(fx))
//    if (r ne problemState) {
//      r.andThen { r =>
//        println(toString(r))
//        r
//      }.orElse(println("Contradiction"))
//    }
//    println("---")
    r

  }

  override def toString(ps: ProblemState) = {
    s"inverse $xOffset/${x.map(ps.dom(_).toString).mkString(", ")} and $yOffset/${y.map(ps.dom(_).toString).mkString(", ")} fx = ${ps(this).bv}"
  }

  def simpleEvaluation: Int = 2
}