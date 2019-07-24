package concrete.constraint.semantic

import bitvectors.BitVector
import concrete.constraint.{Constraint, StatefulConstraint}
import concrete.{Event, Outcome, ProblemState, Variable}

import scala.collection.mutable.ArrayBuffer


/**
  * Constraint x(i) = j => y(j) = i
  *
  * Note the one-sided implication. Post this constraint on both (x, y) and (y, x) to ensure equivalence.
  *
  * y(j) != i => x(i) != j is enforced
  */
final class Inverse(x: Array[Variable], y: Array[Variable], xOffset: Int, yOffset: Int) extends Constraint(x ++ y)
  with StatefulConstraint[BitVector] {

  //  {
  //    val groups = scope.groupBy(identity).filter(_._2.size > 1)
  //    assert(groups.isEmpty, groups.toSeq)
  //  }

  def advise(problemState: ProblemState, event: Event, pos: Int): Int = {
    val bv = problemState(this)
    val card = bv.cardinality
    card * card
  }

  def check(tuple: Array[Int]): Boolean = {
    x.indices.forall { i =>
      val j = tuple(i)
      tuple(j + x.length - yOffset) == i + xOffset
    }
  }

  def init(ps: ProblemState): Outcome = ps.updateState(this, BitVector.filled(x.length))

  def revise(problemState: ProblemState, mod: BitVector): Outcome = {
    //println(s"$mod of ${toString(problemState)}")
    // fx contains "free variables"

    val xMod = new ArrayBuffer[Int]()

    // Contains unassigned variables
    var frees = problemState(this)

    problemState
      .fold(frees) { (ps, xPos) =>
        // First filter X variables
        //println(s"Filtering $xPos")
        //println(ps.currentDomains.toSeq)
        val res = ps.filterDom(x(xPos))(v => ps.dom(y(v - yOffset)).contains(xPos + xOffset))
          .andThen { r =>
            //println(r.currentDomains.toSeq)
            r
          }
        if (res ne ps) {
          xMod += xPos
        }
        res
      }
      .fold(mod.rangeUntil(x.length) ++ xMod) { (ps, xPos) =>
        // Check whether variables in X have been assigned, filter variables in
        // Y accordingly
        val dom = ps.dom(x(xPos))
        if (dom.isAssigned) {
          frees -= xPos

          val xVal = dom.singleValue

          val yPos = xVal - yOffset

          val yVal = xPos + xOffset

          ps.tryAssign(y(yPos), yVal)
        } else {
          ps
        }
      }
      .updateState(this, frees)
  }

  override def toString(ps: ProblemState): String = {
    s"inverse $xOffset/${x.map(ps.dom(_).toString).mkString(", ")} and $yOffset/${y.map(ps.dom(_).toString).mkString(", ")} fx = ${ps(this)}"
  }

  def simpleEvaluation: Int = 2


}