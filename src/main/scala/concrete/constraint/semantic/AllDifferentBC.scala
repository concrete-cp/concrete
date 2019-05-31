package concrete.constraint.semantic

import bitvectors.BitVector
import concrete._
import concrete.constraint.{BC, Constraint, FixPoint}

import scala.annotation.tailrec

final class AllDifferentBC(scope: Array[Variable]) extends Constraint(scope) with BC with AllDiffChecker with FixPoint {

  def this(vars: Variable*) = this(vars.toArray)

  private val t = new Array[Int](2 * arity + 2) // Tree links
  private val d = new Array[Int](2 * arity + 2) // Diffs between critical capacities
  private val h = new Array[Int](2 * arity + 2) // Hall interval links
  private val bounds = new Array[Int](2 * arity + 2)
  private val intervals = Array.tabulate(arity)(p => HInterval(p))
  private val minsorted = intervals.clone
  private val maxsorted = intervals.clone
  def simpleEvaluation = 3
  private val doms: Array[Domain] = new Array(arity)
  private val eval: Int = {
    (31 - Integer.numberOfLeadingZeros(arity)) * arity
  }
  var nbBounds = 0

  def except: Set[Int] = Set()

  def init(ps: ProblemState): ProblemState = ps

  override def revise(ps: ProblemState, mod: BitVector): Outcome = {
    var i = arity - 1
    while (i >= 0) {
      doms(i) = ps.dom(scope(i))
      i -= 1
    }

    fixPoint(ps, { ps =>
      sortIt(doms)
      filterLower(ps, doms)
        .andThen(ps => filterUpper(ps, doms))
    })
  }

  def advise(ps: ProblemState, p: Int): Int = eval

  private def swap(array: Array[HInterval], i: Int, j: Int) {
    val tmp = array(i)
    array(i) = array(j)
    array(j) = tmp
  }

  private def qSortMax(doms: Array[Domain], array: Array[HInterval], from: Int, to: Int) {
    if (from < to) {
      val pivotIndex = (from + to) / 2
      val pivot = array(pivotIndex).ub(doms)
      var left = from
      var right = to
      while (left <= right) {
        while (array(left).ub(doms) < pivot) {
          left += 1
        }
        while (array(right).ub(doms) > pivot) {
          right -= 1
        }
        if (left <= right) {
          swap(array, left, right)
          left += 1
          right -= 1
        }
      }
      qSortMax(doms, array, from, right)
      qSortMax(doms, array, left, to)
    }
  }

  private def qSortMin(doms: Array[Domain], array: Array[HInterval], from: Int, to: Int) {
    if (from < to) {
      val pivotIndex = (from + to) / 2
      val pivot = array(pivotIndex).lb(doms)
      var left = from
      var right = to
      while (left <= right) {
        while (array(left).lb(doms) < pivot) {
          left += 1
        }
        while (array(right).lb(doms) > pivot) {
          right -= 1
        }
        if (left <= right) {
          swap(array, left, right)
          left += 1
          right -= 1
        }
      }
      qSortMin(doms, array, from, right)
      qSortMin(doms, array, left, to)
    }
  }

  private def sortIt(doms: Array[Domain]) {
    qSortMin(doms, minsorted, 0, minsorted.length - 1)
    qSortMax(doms, maxsorted, 0, maxsorted.length - 1)

    val min = minsorted(0).lb(doms)
    var last = min - 2
    var nb = 0
    bounds(0) = last

    @tailrec
    def proceed(min: Int, max: Int, i: Int, j: Int) {
      if (i < arity && min <= max) {
        if (min != last) {
          nb += 1
          last = min
          bounds(nb) = min
        }
        minsorted(i).minrank = nb

        if (i < arity - 1) {
          proceed(minsorted(i + 1).lb(doms), max, i + 1, j)
        } else {
          proceed(min, max, i + 1, j)
        }
      } else {
        if (max != last) {
          nb += 1
          last = max
          bounds(nb) = max
        }
        maxsorted(j).maxrank = nb

        if (j < arity - 1) {
          proceed(min, maxsorted(j + 1).ub(doms) + 1, i, j + 1)
        }
      }
    }

    proceed(min, maxsorted(0).ub(doms) + 1, 0, 0)

    this.nbBounds = nb
    bounds(nb + 1) = bounds(nb) + 2
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

  private def filterLower(ps: ProblemState, doms: Array[Domain]): Outcome = {

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
      val x = maxsorted(i).minrank
      val y = maxsorted(i).maxrank
      var z = pathmax(t, x + 1)
      val j = t(z)

      d(z) -= 1
      if (d(z) == 0) {
        t(z) = z + 1
        z = pathmax(t, t(z))
        t(z) = j
      }

      pathset(t, x + 1, z, z)

      if (d(z) < bounds(z) - bounds(y)) {
        return Contradiction(Seq(scope(i)))
      }

      if (h(x) > x) {
        val w = pathmax(h, h(x))
        val p = maxsorted(i).p
        val dom = mod.dom(scope(p)).removeUntil(bounds(w))
        doms(p) = dom
        mod = mod.updateDomNonEmpty(scope(p), dom)
        pathset(h, x, w, w)
      }

      if (d(z) == bounds(z) - bounds(y)) {
        pathset(h, h(y), j - 1, y)
        h(y) = j - 1
      }
      i += 1
    }
    mod
  }

  private def filterUpper(ps: ProblemState, doms: Array[Domain]): Outcome = {
    var mod = ps
    var i = nbBounds
    while (i >= 0) {
      t(i) = i + 1
      h(i) = i + 1
      d(i) = bounds(i + 1) - bounds(i)
      i -= 1
    }
    i = arity - 1
    while (i >= 0) {
      val x = minsorted(i).maxrank
      val y = minsorted(i).minrank
      var z = pathmin(t, x - 1)
      val j = t(z)

      d(z) -= 1
      if (d(z) == 0) {
        t(z) = z - 1
        z = pathmin(t, t(z))
        t(z) = j
      }

      pathset(t, x - 1, z, z)

      if (d(z) < bounds(y) - bounds(z)) {
        return Contradiction(Seq(scope(i)))
      }

      if (h(x) < x) {
        val w = pathmin(h, h(x))
        val p = minsorted(i).p
        val dom = mod.dom(scope(p)).removeFrom(bounds(w))
        if (dom.isEmpty) {
          return Contradiction(Seq(scope(i)))
        } else {
          doms(p) = dom
          mod = mod.updateDomNonEmpty(scope(p), dom)
        }
        pathset(h, x, w, w)
      }
      if (d(z) == bounds(y) - bounds(z)) {
        pathset(h, h(y), j + 1, y)
        h(y) = j + 1
      }
      i -= 1
    }
    mod
  }

  case class HInterval(p: Int) {
    var minrank: Int = 0
    var maxrank: Int = 0

    def variable = scope(p)

    def lb(doms: Array[Domain]): Int = doms(p).head

    def ub(doms: Array[Domain]): Int = doms(p).last
  }
}
