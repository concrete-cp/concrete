package cspfj.constraint.semantic;

import java.util.Arrays
import java.util.Comparator
import cspfj.problem.Variable
import cspfj.constraint.AbstractConstraint
import cspfj.util.BitVector
import cspfj.problem.EmptyDomainException
import scala.annotation.tailrec
import cspfj.UNSATException

final class HInterval(val v: Variable) {
  var minrank: Int = 0
  var maxrank: Int = 0
}

object MAX extends Ordering[HInterval] {
  def compare(o1: HInterval, o2: HInterval) =
    o1.v.dom.lastValue - o2.v.dom.lastValue
}

object MIN extends Ordering[HInterval] {
  def compare(o1: HInterval, o2: HInterval) =
    o1.v.dom.firstValue - o2.v.dom.firstValue
}

final class BoundAllDiff(vars: Variable*) extends AbstractConstraint(null, vars.toArray) {
  //val PROPAGATE_ON_INSTANTIATIONS = true;
  //val PROPAGATE_ON_BOUNDS = true;

  val t = new Array[Int](2 * arity + 2) // Tree links
  val d = new Array[Int](2 * arity + 2) // Diffs between critical capacities
  val h = new Array[Int](2 * arity + 2) // Hall interval links
  val bounds = new Array[Int](2 * arity + 2)

  var nbBounds = 0

  val intervals = scope.map(new HInterval(_)).toArray
  val minsorted = intervals.clone
  val maxsorted = intervals.clone

  //var infBoundModified = true;
  //var supBoundModified = true;

  def sortIt() {
    Arrays.sort(minsorted, MIN);
    Arrays.sort(maxsorted, MAX);

    var min = minsorted(0).v.dom.firstValue
    var max = maxsorted(0).v.dom.lastValue + 1
    var last = min - 2;
    var nb = 0;
    bounds(0) = last;

    var i = 0
    var j = 0

    @tailrec
    def proceed() {
      if (i < arity && min <= max) {
        if (min != last) {
          nb += 1
          last = min
          bounds(nb) = min
        }
        minsorted(i).minrank = nb;
        i += 1
        if (i < arity) {
          min = minsorted(i).v.dom.firstValue
        }
        proceed()
      } else {
        if (max != last) {
          nb += 1
          last = max
          bounds(nb) = max
        }
        maxsorted(j).maxrank = nb;
        j += 1
        if (j != arity) {
          max = maxsorted(j).v.dom.lastValue + 1;
          proceed()
        }
      }
    }

    proceed()

    this.nbBounds = nb;
    bounds(nb + 1) = bounds(nb) + 2;
  }

  def pathset(tab: Array[Int], start: Int, end: Int, to: Int) {
    var next = start;
    var prev = next;

    while (prev != end) {
      next = tab(prev);
      tab(prev) = to;
      prev = next;
    }
  }

  def pathmin(tab: Array[Int], ip: Int) = {
    var i = ip
    while (tab(i) < i) {
      i = tab(i);
    }
    i;
  }

  def pathmax(tab: Array[Int], ip: Int) = {
    var i = ip
    while (tab(i) > i) {
      i = tab(i);
    }
    i;
  }

  def filterLower() {
    var i = 1
    while (i <= nbBounds + 1) {
      h(i) = i - 1
      t(i) = i - 1;
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
        maxsorted(i).v.dom.removeToVal(bounds(w) - 1);
        pathset(h, x, w, w);
      }

      if (d(z) == bounds(z) - bounds(y)) {
        pathset(h, h(y), j - 1, y);
        h(y) = j - 1;
      }
      i += 1
    }
  }

  def filterUpper() {
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
        minsorted(i).v.dom.removeFromVal(bounds(w));
        pathset(h, x, w, w);
      }
      if (d(z) == bounds(y) - bounds(z)) {
        pathset(h, h(y), j + 1, y);
        h(y) = j + 1;
      }
      i -= 1
    }
  }

  def revise(revCount: Int) {
    //    var left = 0
    //    var right = 0
    //    var j = 0
    //    while (j < arity) {
    //      left = Integer.MIN_VALUE
    //      right = Integer.MIN_VALUE;
    //      var i = 0
    //      while (i < arity) {
    //        if (scope(i).dom.size == 1) {
    //          val value = scope(i).dom.firstValue;
    //          if (i != j) {
    //            if (value == right + 1) {
    //              right = value;
    //            } else {
    //              scope(j).dom.removeValInterval(left, right);
    //              left = value
    //              right = value;
    //            }
    //            // vars(j).removeVal(vars(i).getVal(), this, true);
    //          }
    //        }
    //        i += 1
    //      }
    //      scope(j).dom.removeValInterval(left, right);
    //      j += 1
    //    }

    propagate();
  }

  def propagate() {
    //if (infBoundModified || supBoundModified) {
    sortIt();
    filterLower()
    filterUpper()
    //      filterUpper();
    //      infBoundModified = false;
    //      supBoundModified = false;
    //    }
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

  def getEvaluation = arity

}
