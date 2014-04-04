package concrete.constraint.semantic;

import java.util.Arrays
import java.util.Comparator
import concrete.Variable
import concrete.constraint.Constraint
import concrete.util.BitVector
import scala.annotation.tailrec
import concrete.UNSATException
import concrete.constraint.Removals
import concrete.util.Backtrackable
import scala.collection.immutable.Queue
import concrete.Domain
import concrete.util.IntSet
import concrete.AdviseCount
import scala.collection.mutable.HashSet
import concrete.UNSATObject
import concrete.UNSATException
import concrete.constraint.BC

final case class HInterval(
  val dom: Domain,
  val pos: Int) {
  var minrank: Int = 0
  var maxrank: Int = 0
}

final class AllDifferentBC(vars: Variable*) extends Constraint(vars.toArray) with BC with AllDiffChecker {

  val t = new Array[Int](2 * arity + 2) // Tree links
  val d = new Array[Int](2 * arity + 2) // Diffs between critical capacities
  val h = new Array[Int](2 * arity + 2) // Hall interval links
  val bounds = new Array[Int](2 * arity + 2)

  var nbBounds = 0

  val intervals = scope.zipWithIndex.map { case (v, i) => new HInterval(v.dom, i) }.toArray
  val minsorted = intervals.clone
  val maxsorted = intervals.clone

  private def isSortedMax(array: Array[HInterval], from: Int, to: Int): Boolean = {
    var i = from + 1
    while (i <= to) {
      if (array(i - 1).dom.lastValue > array(i).dom.lastValue) {
        return false
      }
      i += 1
    }
    true
  }

  private def isSortedMin(array: Array[HInterval], from: Int, to: Int): Boolean = {
    var i = from + 1
    while (i <= to) {
      if (array(i - 1).dom.firstValue > array(i).dom.firstValue) {
        return false
      }
      i += 1
    }
    true
  }

  private def swap(array: Array[HInterval], i: Int, j: Int) {
    val tmp = array(i)
    array(i) = array(j)
    array(j) = tmp
  }

  private def qSortMax(array: Array[HInterval], from: Int, to: Int) {
    if (!isSortedMax(array, from, to)) {
      //if (to > from) {
      val pivotIndex = (from + to) / 2
      val pivot = array(pivotIndex).dom.lastValue
      var left = from
      var right = to
      while (left <= right) {
        while (array(left).dom.lastValue < pivot) {
          left += 1
        }
        while (array(right).dom.lastValue > pivot) {
          right -= 1
        }
        if (left <= right) {
          swap(array, left, right)
          left += 1
          right -= 1
        }
      }
      qSortMax(array, from, right)
      qSortMax(array, left, to)
    }
  }

  private def qSortMin(array: Array[HInterval], from: Int, to: Int) {
    if (!isSortedMin(array, from, to)) {
      //if (to > from) {
      val pivotIndex = (from + to) / 2
      val pivot = array(pivotIndex).dom.firstValue
      var left = from
      var right = to
      while (left <= right) {
        while (array(left).dom.firstValue < pivot) {
          left += 1
        }
        while (array(right).dom.firstValue > pivot) {
          right -= 1
        }
        if (left <= right) {
          swap(array, left, right)
          left += 1
          right -= 1
        }
      }
      qSortMin(array, from, right)
      qSortMin(array, left, to)
    }
  }

  private def sortIt() {
    qSortMin(minsorted, 0, minsorted.length - 1)
    qSortMax(maxsorted, 0, maxsorted.length - 1)

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
  private def pathset(tab: Array[Int], start: Int, end: Int, to: Int) {
    if (start != end) {
      val next = tab(start)
      tab(start) = to
      pathset(tab, next, end, to)
    }
  }

  @tailrec
  private def pathmin(tab: Array[Int], i: Int): Int =
    if (tab(i) < i) pathmin(tab, tab(i))
    else i

  @tailrec
  private def pathmax(tab: Array[Int], i: Int): Int =
    if (tab(i) > i) pathmax(tab, tab(i))
    else i

  private def filterLower(): List[Int] = {
    var mod: List[Int] = Nil
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
        throw UNSATObject
      }

      if (h(x) > x) {
        var w = pathmax(h, h(x));
        val ch = maxsorted(i).dom.removeToVal(bounds(w) - 1)
        assert(ch)
        mod ::= maxsorted(i).pos
        pathset(h, x, w, w);
      }

      if (d(z) == bounds(z) - bounds(y)) {
        pathset(h, h(y), j - 1, y);
        h(y) = j - 1;
      }
      i += 1
    }
    mod
  }

  private def filterUpper(): List[Int] = {
    var mod: List[Int] = Nil
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
        throw UNSATObject
      }

      if (h(x) < x) {
        val w = pathmin(h, h(x));
        val ch = minsorted(i).dom.removeFromVal(bounds(w))
        assert(ch)
        mod ::= minsorted(i).pos
        pathset(h, x, w, w);
      }
      if (d(z) == bounds(y) - bounds(z)) {
        pathset(h, h(y), j + 1, y);
        h(y) = j + 1;
      }
      i -= 1
    }
    mod
  }

  def shave() = {
    sortIt()
    filterLower() ++ filterUpper()
  }

  private val eval: Int = (31 - Integer.numberOfLeadingZeros(arity)) * arity

  def advise(p: Int) = eval

  val simpleEvaluation = 3
}
