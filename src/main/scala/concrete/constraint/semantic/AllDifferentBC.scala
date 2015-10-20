package concrete.constraint.semantic;

import scala.annotation.tailrec
import concrete.Contradiction
import concrete.Domain
import concrete.Variable
import concrete.constraint.Constraint
import concrete.constraint.BC
import concrete.ProblemState
import concrete.Outcome

final case class HInterval(
    val v: Variable) {
  var minrank: Int = 0
  var maxrank: Int = 0
  def dom(ps: ProblemState) = ps.dom(v)
}

final class AllDifferentBC(vars: Variable*) extends Constraint(vars.toArray) with BC with AllDiffChecker {

  def init(ps: ProblemState) = ps

  val t = new Array[Int](2 * arity + 2) // Tree links
  val d = new Array[Int](2 * arity + 2) // Diffs between critical capacities
  val h = new Array[Int](2 * arity + 2) // Hall interval links
  val bounds = new Array[Int](2 * arity + 2)

  var nbBounds = 0

  val intervals = (0 until arity).map(p => new HInterval(scope(p))).toArray
  val minsorted = intervals.clone
  val maxsorted = intervals.clone

  private def isSortedMax(ps: ProblemState, array: Array[HInterval], from: Int, to: Int): Boolean = {
    var i = from + 1
    while (i <= to) {
      if (ps.dom(array(i - 1).v).last > ps.dom(array(i).v).last) {
        return false
      }
      i += 1
    }
    true
  }

  private def isSortedMin(ps: ProblemState, array: Array[HInterval], from: Int, to: Int): Boolean = {
    var i = from + 1
    while (i <= to) {
      if (ps.dom(array(i - 1).v).head > ps.dom(array(i).v).head) {
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

  private def qSortMax(ps: ProblemState, array: Array[HInterval], from: Int, to: Int) {
    if (!isSortedMax(ps, array, from, to)) {
      //if (to > from) {
      val pivotIndex = (from + to) / 2
      val pivot = ps.dom(array(pivotIndex).v).last
      var left = from
      var right = to
      while (left <= right) {
        while (array(left).dom(ps).last < pivot) {
          left += 1
        }
        while (array(right).dom(ps).last > pivot) {
          right -= 1
        }
        if (left <= right) {
          swap(array, left, right)
          left += 1
          right -= 1
        }
      }
      qSortMax(ps, array, from, right)
      qSortMax(ps, array, left, to)
    }
  }

  private def qSortMin(ps: ProblemState, array: Array[HInterval], from: Int, to: Int) {
    if (!isSortedMin(ps, array, from, to)) {
      //if (to > from) {
      val pivotIndex = (from + to) / 2
      val pivot = array(pivotIndex).dom(ps).head
      var left = from
      var right = to
      while (left <= right) {
        while (array(left).dom(ps).head < pivot) {
          left += 1
        }
        while (array(right).dom(ps).head > pivot) {
          right -= 1
        }
        if (left <= right) {
          swap(array, left, right)
          left += 1
          right -= 1
        }
      }
      qSortMin(ps, array, from, right)
      qSortMin(ps, array, left, to)
    }
  }

  private def sortIt(ps: ProblemState) {
    qSortMin(ps, minsorted, 0, minsorted.length - 1)
    qSortMax(ps, maxsorted, 0, maxsorted.length - 1)

    val min = minsorted.head.dom(ps).head
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

        if (i < arity - 1) proceed(minsorted(i + 1).dom(ps).head, max, i + 1, j)
        else proceed(min, max, i + 1, j)
      } else {
        if (max != last) {
          nb += 1
          last = max
          bounds(nb) = max
        }
        maxsorted(j).maxrank = nb;

        if (j < arity - 1) {
          proceed(min, maxsorted(j + 1).dom(ps).last + 1, i, j + 1)
        }
      }
    }

    proceed(min, maxsorted(0).dom(ps).last + 1, 0, 0)

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

  private def filterLower(ps: ProblemState): Outcome = {

    var mod = ps

    var i = nbBounds + 1
    while (i >= 1) {
      t(i) = i - 1
      h(i) = i - 1
      d(i) = bounds(i) - bounds(i - 1)
      i -= 1
    }

    assert(i == 0)

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
        val v = maxsorted(i).v
        val dom = ps.dom(v).removeTo(bounds(w) - 1)
        mod = mod.updateDomNonEmpty(v, dom)
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

  private def filterUpper(ps: ProblemState): Outcome = {
    var mod = ps
    var i = nbBounds
    while (i >= 0) {
      t(i) = i + 1
      h(i) = i + 1;
      d(i) = bounds(i + 1) - bounds(i);
      i -= 1
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
        // val dom = ps.dom(id).removeFrom(bounds(w))
        mod.removeFrom(minsorted(i).v, bounds(w)) match {
          case Contradiction    => return Contradiction
          case ps: ProblemState => mod = ps
        }
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

  def shave(ps: ProblemState) = {
    sortIt(ps)
    filterLower(ps) andThen filterUpper
  }

  private val eval: Int = {
    (31 - Integer.numberOfLeadingZeros(arity)) * arity
  }

  def advise(ps: ProblemState, p: Int) = eval

  val simpleEvaluation = 3
}
