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

  private val timestamp = new Timestamp()

  var gNo: Set[Int] = new SparseSet(mdd.identify(timestamp.next()) + 1)

  def restore(data: Set[Int]) {
    gNo = data
  }
  def save = gNo

  // Members declared in concrete.constraint.Constraint
  override def checkIndices(t: Array[Int]) = mdd.contains(t)

  def checkValues(tuple: Array[Int]): Boolean = throw new UnsupportedOperationException

  def simpleEvaluation: Int = math.min(Constraint.NP, scope.count(_.dom.size > 1))

  // Members declared in concrete.constraint.Removals
  val prop = mdd.edges(timestamp.next()).toDouble / doubleCardSize

  def getEvaluation = (prop * doubleCardSize).toInt

  private val unsupported = scope map (p => new collection.mutable.BitSet(p.dom.maxSize))

  var delta: Int = _
  //mdd.registerTimestamp()

  def revise(modified: List[Int]) = {
    for (i <- scope.indices) {
      unsupported(i).clear()
      for (j <- scope(i).dom.indices) {
        unsupported(i) += j
      }
    }

    delta = arity

    val oldGno = gNo

    val sat = seekSupports(timestamp.next(), mdd, 0)
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
    } else if (g.isEmpty) {
      false
    } else if (g.cache.timestamp == ts) {
      true
    } else if (gNo.contains(g.getId)) {
      false
    } else {
      var res = false
      val dom = scope(i).dom
      var ak = dom.first
      while (ak >= 0 && delta > i) {
        val gk = g.subMDD(ak)

        if (seekSupports(ts, gk, i + 1)) {
          res = true
          unsupported(i) -= ak

          if (i + 1 == delta && unsupported(i).isEmpty) {
            delta = i
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

  override def dataSize = mdd.edges(timestamp.next())

}
