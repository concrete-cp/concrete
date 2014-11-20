package concrete.constraint.semantic;

import java.util.Arrays
import java.util.Comparator
import concrete.Variable
import concrete.constraint.Constraint
import concrete.util.BitVector
import scala.annotation.tailrec
import concrete.UNSATException
import concrete.constraint.Removals
import scala.collection.immutable.Queue
import concrete.Domain
import concrete.constraint.AdviseCount
import scala.collection.mutable.HashSet
import concrete.UNSATObject
import concrete.UNSATException
import concrete.constraint.BC
import concrete.constraint.StatelessBC
import concrete.Revised
import concrete.Contradiction
import concrete.ReviseOutcome
import scala.collection.mutable.BitSet

final case class HInterval(
  val pos: Int) {
  var minrank: Int = 0
  var maxrank: Int = 0
}

final class AllDifferentBC(vars: Variable*) extends Constraint(vars.toArray) with StatelessBC with AllDiffChecker {

  val t = new Array[Int](2 * arity + 2) // Tree links
  val d = new Array[Int](2 * arity + 2) // Diffs between critical capacities
  val h = new Array[Int](2 * arity + 2) // Hall interval links
  val bounds = new Array[Int](2 * arity + 2)

  var nbBounds = 0

  val intervals = (0 until arity).map(new HInterval(_)).toArray
  val minsorted = intervals.clone
  val maxsorted = intervals.clone

  private def isSortedMax(domains: IndexedSeq[Domain], array: Array[HInterval], from: Int, to: Int): Boolean = {
    var i = from + 1
    while (i <= to) {
      if (domains(array(i - 1).pos).last > domains(array(i).pos).last) {
        return false
      }
      i += 1
    }
    true
  }

  private def isSortedMin(domains: IndexedSeq[Domain], array: Array[HInterval], from: Int, to: Int): Boolean = {
    var i = from + 1
    while (i <= to) {
      if (domains(array(i - 1).pos).head > domains(array(i).pos).head) {
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

  private def qSortMax(domains: IndexedSeq[Domain], array: Array[HInterval], from: Int, to: Int) {
    if (!isSortedMax(domains, array, from, to)) {
      //if (to > from) {
      val pivotIndex = (from + to) / 2
      val pivot = domains(array(pivotIndex).pos).last
      var left = from
      var right = to
      while (left <= right) {
        while (domains(array(left).pos).last < pivot) {
          left += 1
        }
        while (domains(array(right).pos).last > pivot) {
          right -= 1
        }
        if (left <= right) {
          swap(array, left, right)
          left += 1
          right -= 1
        }
      }
      qSortMax(domains, array, from, right)
      qSortMax(domains, array, left, to)
    }
  }

  private def qSortMin(domains: IndexedSeq[Domain], array: Array[HInterval], from: Int, to: Int) {
    if (!isSortedMin(domains, array, from, to)) {
      //if (to > from) {
      val pivotIndex = (from + to) / 2
      val pivot = domains(array(pivotIndex).pos).head
      var left = from
      var right = to
      while (left <= right) {
        while (domains(array(left).pos).head < pivot) {
          left += 1
        }
        while (domains(array(right).pos).head > pivot) {
          right -= 1
        }
        if (left <= right) {
          swap(array, left, right)
          left += 1
          right -= 1
        }
      }
      qSortMin(domains, array, from, right)
      qSortMin(domains, array, left, to)
    }
  }

  private def sortIt(domains: IndexedSeq[Domain]) {
    qSortMin(domains, minsorted, 0, minsorted.length - 1)
    qSortMax(domains, maxsorted, 0, maxsorted.length - 1)

    val min = domains(minsorted(0).pos).head
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

        if (i < arity - 1) proceed(domains(minsorted(i + 1).pos).head, max, i + 1, j)
        else proceed(min, max, i + 1, j)
      } else {
        if (max != last) {
          nb += 1
          last = max
          bounds(nb) = max
        }
        maxsorted(j).maxrank = nb;

        if (j < arity - 1) {
          proceed(min, domains(maxsorted(j + 1).pos).last + 1, i, j + 1)
        }
      }
    }

    proceed(min, domains(maxsorted(0).pos).last + 1, 0, 0)

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

  private def filterLower(domains: IndexedSeq[Domain]): ReviseOutcome[Unit] = {
    val mod = domains.toArray

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
        return Contradiction
      }

      if (h(x) > x) {
        var w = pathmax(h, h(x));
        mod(maxsorted(i).pos) = domains(maxsorted(i).pos).removeTo(bounds(w) - 1)
        pathset(h, x, w, w);
      }

      if (d(z) == bounds(z) - bounds(y)) {
        pathset(h, h(y), j - 1, y);
        h(y) = j - 1;
      }
      i += 1
    }
    Revised(mod)
  }

  private def filterUpper(domains: IndexedSeq[Domain]): ReviseOutcome[Unit] = {
    val mod = domains.toArray
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
        return Contradiction
      }

      if (h(x) < x) {
        val w = pathmin(h, h(x));
        mod(minsorted(i).pos) = mod(minsorted(i).pos).removeFrom(bounds(w))
        pathset(h, x, w, w);
      }
      if (d(z) == bounds(y) - bounds(z)) {
        pathset(h, h(y), j + 1, y);
        h(y) = j + 1;
      }
      i -= 1
    }
    Revised(mod)
  }

  def shave(domains: IndexedSeq[Domain]) = {
    sortIt(domains)
    filterLower(domains) andThen ((d, _) => filterUpper(d))
  }

  private val eval: Int = (31 - Integer.numberOfLeadingZeros(arity)) * arity

  def advise(domains: IndexedSeq[Domain], p: Int) = eval

  val simpleEvaluation = 3
}
