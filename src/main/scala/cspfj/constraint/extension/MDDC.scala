package cspfj.constraint.extension

import cspfj.util.Backtrackable
import cspfj.constraint.Removals
import cspfj.Variable
import cspfj.constraint.Constraint
import cspfj.util.SparseSet
import cspfj.util.BitVector
import scala.util.control.Breaks._

class MDDC(_scope: Array[Variable], private val mdd: MDD)
  extends Constraint(_scope) with Removals with Backtrackable[Set[Int]] {

  override def setLvl(l: Int) {
    super.setLvl(l)
    setLevel(l)
  }

  override def restoreLvl(l: Int) {
    super.restoreLvl(l)
    restoreLevel(l)
  }

  var gNo: Set[Int] = new SparseSet(mdd.identify() + 1)

  // Members declared in cspfj.util.Backtrackable 
  def restore(data: Set[Int]) {
    gNo = data
  }
  def save = gNo

  // Members declared in cspfj.constraint.Constraint
  override def checkIndices(t: Array[Int]) = mdd.contains(t)

  def checkValues(tuple: Array[Int]): Boolean = throw new UnsupportedOperationException

  def simpleEvaluation: Int = math.min(7, scope.count(_.dom.size > 1))

  // Members declared in cspfj.constraint.Removals
  val getEvaluation: Int = mdd.nodes

  private val unsupported = scope map (p => BitVector.newBitVector(p.dom.maxSize))

  var delta: Int = _

  def revise(modified: List[Int]): Boolean = {
    for (i <- scope.indices) {
      unsupported(i).fill(false)
      for (j <- scope(i).dom.indices) {
        unsupported(i).set(j)
      }
    }

    delta = arity

    MDD.timestamp += 1

    seekSupports(MDD.timestamp, mdd, 0)

    //    val (_, newNo, delta) = mdd.seekSupports(MDD.timestamp, scope, unsupported, 0, gNo, arity)
    //   
    //    if (gNo ne newNo) {
    //      altering()
    //      gNo = newNo
    //    }
    //   

    altering()

    (0 until delta).foldLeft(false)(
      (acc, p) => scope(p).dom.filter(i => !unsupported(p)(i)) || acc)
  }

  private def seekSupports(ts: Int, g: MDD, i: Int): Boolean = {
    if (g eq MDDLeaf) {
      if (i < delta) {
        delta = i
      }
      true
    } else if (g.timestamp == ts) {
      true
    } else if (gNo.contains(g.getId)) {
      false
    } else {
      var res = false

      g.forSubtries {
        (ak, gk) =>
          ExtensionConstraintReduceable.checks += 1
          if (scope(i).dom.present(ak) && seekSupports(ts, gk, i + 1)) {
            res = true
            unsupported(i).clear(ak)

            if (i + 1 == delta && unsupported(i).isEmpty) {
              delta = i
              false
            } else {
              true
            }
          } else {
            true
          }

      }
      if (res) {
        g.timestamp = ts
      } else {
        gNo += g.getId
      }
      res
    }
  }

}