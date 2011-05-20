package cspfj.problem;

import java.util.Arrays
import com.google.common.base.Preconditions
import cspfj.problem.AbstractDomain
import cspfj.util.Arrays2
import cspfj.util.BitVector;
import scala.collection.JavaConversions

object BitVectorDomain {
  val HISTORY_INCREMENT = 10
}

final class BitVectorDomain(
  val domain: IndexedSeq[Int],
  val bvDomain: BitVector,
  var bvHistory: Array[BitVector],
  var dsHistory: Array[Int]) extends AbstractDomain with Cloneable {

  override val index = domain.indices.map(i => domain(i) -> i).toMap
  override var size = domain.size;
  private var _last = size - 1;
  var currentLevel = 0;

  def this(dom: Int*) = {
    this(dom.toIndexedSeq, BitVector.newBitVector(dom.size, true), new Array[BitVector](BitVectorDomain.HISTORY_INCREMENT), new Array[Int](BitVectorDomain.HISTORY_INCREMENT))
    require(dom.sliding(2).foldLeft(true)((test, v) => test && v(0) < v(1)),
      "Only ordered domains are supported");
  }

  def this(copy: BitVectorDomain) = {
    this(copy.domain, copy.bvDomain.clone, copy.bvHistory.clone, copy.dsHistory.clone)

  }

  override def first = bvDomain.nextSetBit(0);

  override def last = {
    assert(_last == bvDomain.prevSetBit(domain.size), "Recorded " + last
      + ", should be " + bvDomain.prevSetBit(domain.size)
      + " given current domain " + toString());
    _last;
  }

  override def lastAbsent() = bvDomain.prevClearBit(domain.size);

  override def next(i: Int) = bvDomain.nextSetBit(i + 1);

  override def prev(i: Int) = bvDomain.prevSetBit(i);

  override def prevAbsent(i: Int) = bvDomain.prevClearBit(i);

  /**
   * @param value
   *            the value we seek the index for
   * @return the index of the given value or -1 if it could not be found
   */
  //def index(value: Int) = domain.indexOf(value)

  override def greatest(value: Int) = {
    var lb = first();
    if (this.value(lb) > value) {
      -1;
    } else {
      var ub = last();
      while (this.value(ub) > value) {
        val test = (ub + lb) / 2;
        if (this.value(test) > value) {
          ub = test - 1;
        } else {
          lb = test + 1;
        }
      }
      ub;
    }
  }

  override def lowest(value: Int) = {
    var ub = last();
    if (this.value(ub) < value) {
      -1;
    } else {
      var lb = first();
      while (this.value(lb) < value) {
        val test = (ub + lb) / 2;
        if (this.value(test) >= value) {
          ub = test - 1;
        } else {
          lb = test + 1;
        }
      }
      lb;
    }
  }

  /**
   * @param index
   *            index to test
   * @return true iff index is present
   */
  override def present(index: Int) = bvDomain.get(index);

  override def setSingle(index: Int) {
    bvDomain.setSingle(index);
    size = 1;
    _last = index;
    if (bvHistory(currentLevel) == null) {
      bvHistory(currentLevel) = BitVector.newBitVector(domain.length,
        false);
    }
  }

  def value(index: Int) = domain(index);

  override def remove(index: Int) {
    assert(present(index));
    size -= 1;
    bvDomain.clear(index);
    if (index == last) {
      _last = bvDomain.prevSetBit(index);
    }
    assert(last == bvDomain.prevSetBit(domain.length), "Recorded " + last
      + ", should be " + bvDomain.prevSetBit(domain.length)
      + " given current domain " + toString());

    if (bvHistory(currentLevel) == null) {
      bvHistory(currentLevel) = BitVector.newBitVector(domain.length,
        false);
    }
  }

  override def removeFrom(lb: Int) = {
    val removed = bvDomain.clearFrom(lb);
    if (removed > 0) {
      _last = bvDomain.prevSetBit(lb);
      if (bvHistory(currentLevel) == null) {
        bvHistory(currentLevel) = BitVector.newBitVector(domain.length,
          false);
      }
    }
    size -= removed;
    removed;
  }

  override def removeTo(ub: Int) {
    val removed = bvDomain.clearTo(ub + 1);
    if (removed > 0 && bvHistory(currentLevel) == null) {
      bvHistory(currentLevel) = BitVector.newBitVector(domain.length,
        false);
    }
    size -= removed;
    removed;
  }

  val getBitVector = bvDomain;

  def copy() = clone();

  def clone() = new BitVectorDomain(this)

  override val maxSize = domain.length;

  override def setLevel(level: Int) {
    assert(level > currentLevel, "Given level " + level
      + " should be greater than current " + currentLevel);
    ensureCapacity(level);
    if (bvHistory(currentLevel) != null) {
      bvDomain.copyTo(bvHistory(currentLevel));
      dsHistory(currentLevel) = size;
    }
    currentLevel = level;

  }

  private def ensureCapacity(size: Int) {
    if (size >= bvHistory.length) {
      bvHistory = Arrays.copyOf(bvHistory, size + BitVectorDomain.HISTORY_INCREMENT);
      dsHistory = Arrays.copyOf(dsHistory, size + BitVectorDomain.HISTORY_INCREMENT);
    }
  }

  override def restoreLevel(level: Int) {
    assert(level < currentLevel);
    var change = false;
    for (l <- (level + 1 to currentLevel)) {
      if (bvHistory(l) != null) {
        change = true;
        bvHistory(l) = null;
      }
    }
    if (change) {
      var l = level;
      while (l >= 0 && bvHistory(l) == null) {
        l -= 1;
      }
      if (l < 0) {
        bvDomain.fill(true);
        size = domain.length;
      } else {
        assert(!bvHistory(l).isEmpty)
        bvHistory(l).copyTo(bvDomain);
        size = dsHistory(l);

      }
    }
    currentLevel = level;
    _last = bvDomain.prevSetBit(domain.length);

  }

  override def getAtLevel(level: Int) {
    if (level < currentLevel) {
      var l = level;
      while (l >= 0 && bvHistory(l) == null) {
        l -= 1;
      }
      if (l < 0) {
        BitVector.newBitVector(domain.length, true);
      } else {
        bvHistory(l)
      }
    } else bvDomain;
  }

  override val allValues = domain;

  override def currentValues = {
    val values = new Array[Int](size);
    var j = 0;
    var i = first;
    while (i != -1) {
      values(j) = value(i)
      i = next(i)
      j += 1
    }
    values;
  }

  val DISPLAYED_VALUES = 5

  override def toString = {
    val itr = JavaConversions.asScalaIterator(this.iterator);
    if (size <= DISPLAYED_VALUES) {
      itr.mkString("[", ", ", "]");
    } else {
      itr.take(DISPLAYED_VALUES).mkString("[", ", ", " (" + (size - DISPLAYED_VALUES) + " more)]")
    }
  }

}
