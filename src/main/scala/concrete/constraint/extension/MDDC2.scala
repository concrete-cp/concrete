package concrete.constraint.extension

import concrete.UNSATObject
import concrete.Variable
import concrete.constraint.Constraint
import concrete.constraint.Removals
import concrete.util.Backtrackable
import concrete.util.BitVector
import concrete.util.SetWithMax
import concrete.util.SparseSet
import concrete.util.Timestamp

class MDDC2(_scope: Array[Variable], private val mdd: MDD)
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

  // Members declared in concrete.util.Backtrackable
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

  private val unsupported = scope map (p => BitVector.newBitVector(p.dom.maxSize))

  var delta: Int = _

  def revise(modified: List[Int]) = {
    for (i <- scope.indices) {
      unsupported(i).fill(false)
      for (j <- scope(i).dom.indices) {
        unsupported(i).set(j)
      }
    }

    //delta = arity

    val oldGno = gNo

    mark(timestamp.next(), mdd, 0, modified.reverse)

    if (gNo(mdd.getId)) {
      throw UNSATObject
    }
    if (gNo ne oldGno) {
      altering()
    }

    val l = new SetWithMax(arity)
    fillFound(timestamp.next(), mdd, 0, l)

    val c = l.filter(p => scope(p).dom.filter(i => !unsupported(p)(i)))
    if (isFree) {
      entail()
    }
    c
  }

  private def mark(ts: Int, g: MDD, i: Int, mod: List[Int]): Boolean = {
    if (g.cache.timestamp == ts) {
      true
    } else if (gNo.contains(g.getId)) {
      false
    } else {
      mod match {
        case Nil => true
        case `i` :: next => mark2(ts, g, i, next)
        case next: List[Int] => mark2(ts, g, i, next)
      }
    }
  }

  private def mark2(ts: Int, g: MDD, i: Int, next: List[Int]): Boolean = {
    var res = false
    g.forSubtries {
      (ak, gk) =>
        res |= scope(i).dom.present(ak) && mark(ts, gk, i + 1, next)
        true
    }

    if (res) {
      g.cache.timestamp = ts
      //require(isValid(g, i))
    } else {
      gNo += g.getId
    }
    res
  }

  private def fillFound(ts: Int, g: MDD, i: Int, l: SetWithMax) {
    if (g ne MDDLeaf) g.cache(ts, Unit,
      if (i <= l.max) {
        g.forSubtries {
          (ak, gk) =>
            if (scope(i).dom.present(ak) && !gNo(gk.getId)) {
              //require(isValid(gk, i + 1))
              if (unsupported(i).clear(ak) && unsupported(i).isEmpty) {
                l -= i
              }
              fillFound(ts, gk, i + 1, l)
            }
            i <= l.max
        }
      })
  }

  override def dataSize = mdd.edges(timestamp.next())

}