package cspfj.problem;

import cspfj.util.BitVector;
import java.util.Arrays

final object BitVectorDomain {
  val HISTORY_INCREMENT = 20;
  val DISPLAYED_VALUES = 3;
}

final class BitVectorDomain(
  val domain: IndexedSeq[Int],
  private val bvDomain: BitVector,
  private var bvHistory: Array[BitVector],
  private var dsHistory: Array[Int]) extends Domain {
  require(domain.sliding(2).forall(p => p(1) < p(2)), "Only ordered domains are supported");

  val indices = domain map { i => domain(i) -> i } toMap

  private var _size = domain.size

  override def size = _size

  private var _last = domain.size - 1

  private var currentLevel = 0;

  def this(domain: Int*) = this(
    domain.toArray,
    BitVector.newBitVector(domain.length, true),
    new Array[BitVector](BitVectorDomain.HISTORY_INCREMENT),
    new Array[Int](BitVectorDomain.HISTORY_INCREMENT))

  def this(domain: BitVectorDomain) =
    this(domain.domain, domain.bvDomain.clone, domain.bvHistory.clone, domain.dsHistory.clone)

  override def firstIndex = bvDomain.nextSetBit(0);

  override def lastIndex = {
    assert(_last == bvDomain.prevSetBit(domain.length), "Recorded " + last
      + ", should be " + bvDomain.prevSetBit(domain.length)
      + " given current domain " + toString())
    _last;
  }

  override def lastAbsent() = bvDomain.prevClearBit(domain.length);

  override def next(i: Int) = bvDomain.nextSetBit(i + 1)

  override def prev(i: Int) = bvDomain.prevSetBit(i)

  override def prevAbsent(i: Int) = bvDomain.prevClearBit(i)

  /**
   * @param value
   *            the value we seek the index for
   * @return the index of the given value or -1 if it could not be found
   */
  override def index(value: Int) = indices(value)

  private def greatest(value: Int, lb: Int, ub: Int): Int = {
    if (domain(ub) > value) {
      ub
    } else {
      val test = (ub + lb) / 2
      if (domain(test) > value) {
        greatest(value, lb, test - 1)
      } else {
        greatest(value, test + 1, ub)
      }
    }
  }

  override def greatest(value: Int) = {
    var lb = head;
    if (domain(lb) > value) {
      -1;
    } else {
      greatest(value, lb, last);
    }
  }

  private def lowest(value: Int, lb: Int, ub: Int): Int = {
    if (domain(lb) >= value) {
      lb
    } else {
      val test = (ub + lb) / 2;
      if (domain(test) >= value) {
        lowest(value, lb, test - 1)
      } else {
        lowest(value, test + 1, ub)
      }
    }
  }

  override def lowest(value: Int) = {
    var ub = last;
    if (domain(ub) < value) {
      -1;
    } else {
      lowest(value, head, ub)
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
    _size = 1;
    _last = index;
    if (bvHistory(currentLevel) == null) {
      bvHistory(currentLevel) = BitVector.newBitVector(domain.length,
        false);
    }
  }

  override def value(index: Int) =
    domain(index);

  override def remove(index: Int) {
    assert(present(index));
    _size -= 1;
    bvDomain.clear(index);
    if (index == _last) {
      _last = bvDomain.prevSetBit(index);
    }
    assert(_last == bvDomain.prevSetBit(domain.length), "Recorded " + last
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
    _size -= removed;
    removed;
  }

  override def removeTo(ub: Int) = {
    val removed = bvDomain.clearTo(ub + 1);
    if (removed > 0 && bvHistory(currentLevel) == null) {
      bvHistory(currentLevel) = BitVector.newBitVector(domain.length,
        false);
    }
    _size -= removed;
    removed;
  }

  override def getBitVector() = bvDomain

  override val maxSize = domain.length

  override def setLevel(level: Int) {
    assert(level > currentLevel, "Given level " + level
      + " should be greater than current " + currentLevel)
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
        _size = domain.length;
      } else {
        assert(!bvHistory(l).isEmpty)
        bvHistory(l).copyTo(bvDomain);
        _size = dsHistory(l);

      }
    }
    currentLevel = level;
    _last = bvDomain.prevSetBit(domain.length);

  }

  override def getAtLevel(level: Int) = {
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

  override val allValues = domain.toArray;

  override def currentValues = {
    val values = new Array[Int](size);
    var j = 0;
    var i = head;
    while (i != -1) {
      values(j) = value(i)
      i = next(i)
      j += 1
    }
    values;
  }

  override def toString = {
    if (size <= BitVectorDomain.DISPLAYED_VALUES) {
      mkString("[", ", ", "]");
    } else {
      take(BitVectorDomain.DISPLAYED_VALUES).mkString("[", ", ", " (" + (size - BitVectorDomain.DISPLAYED_VALUES) + " more)]")
    }
  }
}
