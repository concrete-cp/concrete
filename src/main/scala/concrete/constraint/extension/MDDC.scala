package concrete.constraint.extension

import concrete.util.Backtrackable
import concrete.constraint.Removals
import concrete.Variable
import concrete.constraint.Constraint
import concrete.util.SparseSet
import concrete.util.BitVector
import concrete.UNSATException
import concrete.UNSATObject
import concrete.util.Timestamp

/* MDDRelation comes with its own timestamp */
class MDDC(_scope: Array[Variable], private val mdd: MDDRelation)
  extends Constraint(_scope) with Removals with Backtrackable[Set[Int]] {

  override def setLvl(l: Int) {
    super.setLvl(l)
    setLevel(l)
  }

  override def restoreLvl(l: Int) {
    super.restoreLvl(l)
    restoreLevel(l)
  }

  var gNo: Set[Int] = new SparseSet(mdd.identify + 1)

  def restore(data: Set[Int]) {
    gNo = data
  }
  def save = gNo

  // Members declared in concrete.constraint.Constraint
  override def checkIndices(t: Array[Int]) = mdd.contains(t)

  def checkValues(tuple: Array[Int]): Boolean = throw new UnsupportedOperationException

  def simpleEvaluation: Int = math.min(Constraint.NP, scope.count(_.dom.size > 1))

  // Members declared in concrete.constraint.Removals
  val prop = mdd.edges.toDouble / doubleCardSize

  def getEvaluation = (prop * doubleCardSize).toInt

  private val unsupported = scope.map(p => new collection.mutable.BitSet(p.dom.maxSize))

  var delta: Int = _

  def revise(modified: List[Int]) = {
    for (i <- scope.indices) {
      unsupported(i).clear()
      unsupported(i) ++= scope(i).dom.indices
      //      for (j <- scope(i).dom.indices) {
      //        unsupported(i) += j
      //      }
    }

    delta = arity

    val oldGno = gNo

    val sat = seekSupports(mdd.timestamp.next(), mdd.mdd, 0)
    if (!sat) {
      throw UNSATObject
    }

    if (gNo ne oldGno) {
      altering()
    }

    val c = (delta - 1 to 0 by -1).filter(p => scope(p).dom.filter(i => !unsupported(p)(i)))
    if (isFree) {
      entail()
    }
    c
  }

  private def seekSupports(ts: Int, g: MDD, i: Int): Boolean = {
    if (g eq MDDLeaf) {
      if (i < delta) {
        delta = i
      }
      true
    } else if (g eq MDD0) {
      false
    } else if (g.cache.timestamp == ts) {
      true
    } else if (gNo.contains(g.getId)) {
      false
    } else {
      var res = false
      val dom = scope(i).dom
      var ak = dom.first
      var continue = true
      while (ak >= 0 && continue) {
        val gk = g.subMDD(ak)

        if (seekSupports(ts, gk, i + 1)) {
          res = true
          unsupported(i) -= ak

          if (i + 1 == delta && unsupported(i).isEmpty) {
            delta = i
            continue = false
          }
        }

        ak = dom.next(ak)
      }
      if (res) {
        g.cache.timestamp = ts
      } else {
        gNo += g.getId
      }
      res
    }
  }

  override def dataSize = mdd.edges

}
