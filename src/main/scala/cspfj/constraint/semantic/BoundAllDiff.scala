package cspfj.constraint.semantic;

import java.util.Arrays
import java.util.Comparator
import cspfj.problem.Variable
import cspfj.constraint.AbstractConstraint
import cspfj.util.BitVector
import cspfj.problem.EmptyDomainException
import scala.annotation.tailrec
import cspfj.UNSATException
import cspfj.constraint.Removals
import cspfj.util.Backtrackable
import scala.collection.immutable.Queue
import cspfj.util.UOList
import cspfj.problem.Domain

final class HInterval(
  val dom: Domain,
  val pos: Int) {
  var minrank: Int = 0
  var maxrank: Int = 0
}

object MAX extends Ordering[HInterval] {
  def compare(o1: HInterval, o2: HInterval) =
    o1.dom.lastValue - o2.dom.lastValue
}

object MIN extends Ordering[HInterval] {
  def compare(o1: HInterval, o2: HInterval) =
    o1.dom.firstValue - o2.dom.firstValue
}

final class BoundAllDiff(vars: Variable*) extends AbstractConstraint(null, vars.toArray) {
  //val PROPAGATE_ON_INSTANTIATIONS = true;
  //val PROPAGATE_ON_BOUNDS = true;

  val t = new Array[Int](2 * arity + 2) // Tree links
  val d = new Array[Int](2 * arity + 2) // Diffs between critical capacities
  val h = new Array[Int](2 * arity + 2) // Hall interval links
  val bounds = new Array[Int](2 * arity + 2)

  var nbBounds = 0

  val intervals = scope.indices.map(i => new HInterval(scope(i).dom, i)).toArray
  val minsorted = intervals.clone
  val maxsorted = intervals.clone

  //var infBoundModified = true;
  //var supBoundModified = true;

  private def bSort[A](array: Array[A], c: Ordering[A]) {

    def swap[A](i: Int) {
      val t = array(i)
      array(i) = array(i + 1)
      array(i + 1) = t
    }

    @tailrec
    def s(end: Int, i: Int, change: Int) {
      if (end > 0) {
        if (i >= end) s(change, 0, 0)
        else if (c.compare(array(i), array(i + 1)) > 0) {
          swap(i)
          s(end, i + 1, i)
        } else s(end, i + 1, change)
      }

    }

    s(array.size - 1, 0, 0)
  }

  def sortIt() {
    bSort(minsorted, MIN)
    bSort(maxsorted, MAX)

    val min = minsorted(0).dom.firstValue
    var last = min - 2;
    var nb = 0;
    bounds(0) = last;

    @tailrec
    def proceed(min: Int, max: Int, i: Int, j: Int) {
      if (i < arity && min <= max) {
        if (min != last) {
          nb += 1
          last = min
          bounds(nb) = min
        }
        minsorted(i).minrank = nb;

        if (i < arity - 1) proceed(minsorted(i + 1).dom.firstValue, max, i + 1, j)
        else proceed(min, max, i + 1, j)
      } else {
        if (max != last) {
          nb += 1
          last = max
          bounds(nb) = max
        }
        maxsorted(j).maxrank = nb;

        if (j < arity - 1) {
          proceed(min, maxsorted(j + 1).dom.lastValue + 1, i, j + 1)
        }
      }
    }

    proceed(min, maxsorted(0).dom.lastValue + 1, 0, 0)

    this.nbBounds = nb;
    bounds(nb + 1) = bounds(nb) + 2;
  }

  @tailrec
  def pathset(tab: Array[Int], start: Int, end: Int, to: Int) {
    if (start != end) {
      val next = tab(start)
      tab(start) = to
      pathset(tab, next, end, to)
    }
  }

  @tailrec
  def pathmin(tab: Array[Int], i: Int): Int =
    if (tab(i) < i) pathmin(tab, tab(i))
    else i

  @tailrec
  def pathmax(tab: Array[Int], i: Int): Int =
    if (tab(i) > i) pathmax(tab, tab(i))
    else i

  def filterLower(): Boolean = {
    var hole = false
    var i = 1
    while (i <= nbBounds + 1) {
      t(i) = i - 1
      h(i) = i - 1
      d(i) = bounds(i) - bounds(i - 1)
      i += 1
    }
    i = 0
    while (i < arity) {
      val x = maxsorted(i).minrank;
      val y = maxsorted(i).maxrank;
      var z = pathmax(t, x + 1);
      val j = t(z);

      d(z) -= 1
      if (d(z) == 0) {
        t(z) = z + 1;
        z = pathmax(t, t(z));
        t(z) = j;
      }

      pathset(t, x + 1, z, z);

      if (d(z) < bounds(z) - bounds(y)) {
        throw UNSATException.e
      }

      if (h(x) > x) {
        var w = pathmax(h, h(x));
        maxsorted(i).dom.removeToVal(bounds(w) - 1)
        hole |= maxsorted(i).dom.firstValue > bounds(w)
        pathset(h, x, w, w);
      }

      if (d(z) == bounds(z) - bounds(y)) {
        pathset(h, h(y), j - 1, y);
        h(y) = j - 1;
      }
      i += 1
    }
    hole
  }

  def filterUpper() = {
    var hole = false
    var i = 0
    while (i <= nbBounds) {
      t(i) = i + 1
      h(i) = i + 1;
      d(i) = bounds(i + 1) - bounds(i);
      i += 1
    }
    i = arity - 1
    while (i >= 0) {
      val x = minsorted(i).maxrank;
      val y = minsorted(i).minrank;
      var z = pathmin(t, x - 1);
      val j = t(z);

      d(z) -= 1
      if (d(z) == 0) {
        t(z) = z - 1;
        z = pathmin(t, t(z));
        t(z) = j;
      }

      pathset(t, x - 1, z, z);

      if (d(z) < bounds(y) - bounds(z)) {
        throw UNSATException.e
      }

      if (h(x) < x) {
        val w = pathmin(h, h(x));
        minsorted(i).dom.removeFromVal(bounds(w))
        hole |= minsorted(i).dom.lastValue < bounds(w) - 1
        pathset(h, x, w, w);
      }
      if (d(z) == bounds(y) - bounds(z)) {
        pathset(h, h(y), j + 1, y);
        h(y) = j + 1;
      }
      i -= 1
    }
    hole
  }

  def filterB(scope: Array[Variable], checkedVariable: Int, value: Int): UOList[Int] = {

    def r(i: Int, mod: UOList[Int]): UOList[Int] = {
      if (i < 0) mod
      else if (i == checkedVariable) r(i - 1, mod)
      else {
        val v = scope(i)
        val index = v.dom.index(value)
        if (index >= 0 && (v.dom.first == index || v.dom.last == index)) {
          v.dom.remove(index)
          r(i - 1, mod + i)
        } else r(i - 1, mod)

      }
    }

    r(scope.size - 1, UOList.empty)

  }

  def revise(r: Int) {
    //    @tailrec
    //    def rev(q: UOList[Int]) {
    //      if (!q.isEmpty) {
    //        val checkedVariable = q.head
    //        val newQueue = q.tail
    //
    //        val value = scope(checkedVariable).dom.firstValue
    //
    //        val modified = filterB(scope, checkedVariable, value)
    //
    //        rev(newQueue ++ modified.filter(scope(_).dom.size == 1))
    //      }
    //    }
    //
    //    rev(UOList.build(change.filter(scope(_).dom.size == 1)))

    //val mod = 
    if (propagate()) revise(r)
    //if (!mod.isEmpty) revise(mod.toList)
  }

  def propagate() = {
    sortIt();
    val hl = filterLower()
    val hu = filterUpper()
    hl || hu
  }

  private val offset = (scope map { _.dom.allValues.min } min)
  private val unionSize = 1 +
    (scope map { _.dom.allValues.last } max) -
    offset

  def check: Boolean = {
    val union = BitVector.newBitVector(unionSize)
    tupleValues.exists { v =>
      if (union.get(v - offset)) return false
      union.set(v - offset)
      true
    }
  }

  def getEvaluation = arity * arity

}
