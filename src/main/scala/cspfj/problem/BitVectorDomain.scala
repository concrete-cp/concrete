package cspfj.problem;

import cspfj.util.BitVector;
import java.util.Arrays

final object BitVectorDomain {
  val HISTORY_INCREMENT = 20;
  val DISPLAYED_VALUES = 3;
}

final class BitVectorDomain(
  private val domain: IndexedSeq[Int],
  private val bvDomain: BitVector,
  /**
   * History entry is (level, domain, domain size)
   */
  private var history: List[(Int, BitVector, Int)]) extends Domain {
  require(domain.sliding(2).forall(p => p.size == 1 || p(0) < p(1)), "Only ordered domains are supported");

  private val indices = domain.zipWithIndex.map { case (v, i) => v -> i }.toMap.withDefaultValue(-1)

  private var _size = domain.size

  override def size = _size

  //  private var _last = domain.size - 1

  private var currentLevel = 0;

  private var removed = false;

  def this(domain: Int*) = this(
    domain.toArray,
    BitVector.newBitVector(domain.length, true),
    Nil)

  def this(domain: BitVectorDomain) =
    this(domain.domain, domain.bvDomain.clone, domain.history)

  override def firstIndex = bvDomain.nextSetBit(0);

  override def lastIndex = bvDomain.prevSetBit(maxSize)

  //  override def lastIndex = {
  //    assert(_last == bvDomain.prevSetBit(domain.length), "Recorded " + last
  //      + ", should be " + bvDomain.prevSetBit(domain.length)
  //      + " given current domain " + toString())
  //    _last;
  //  }

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

  private def closestLeq(value: Int, lb: Int, ub: Int): Int = {
    if (domain(ub) <= value) {
      ub
    } else {
      val test = (ub + lb) / 2
      if (domain(test) > value) {
        closestLeq(value, lb, test - 1)
      } else {
        closestLeq(value, test + 1, ub)
      }
    }
  }

  override def closestLeq(value: Int) = {
    var lb = firstIndex;
    if (domain(lb) > value) {
      -1;
    } else {
      closestLeq(value, lb, lastIndex);
    }
  }

  private def closestGeq(value: Int, lb: Int, ub: Int): Int = {
    if (domain(lb) >= value) {
      lb
    } else {
      val test = (ub + lb) / 2;
      if (domain(test) >= value) {
        closestGeq(value, lb, test - 1)
      } else {
        closestGeq(value, test + 1, ub)
      }
    }
  }

  override def closestGeq(value: Int) = {
    var ub = last;
    if (domain(ub) < value) {
      -1;
    } else {
      closestGeq(value, head, ub)
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
    //_last = index;
    removed = true
  }

  override def value(index: Int) =
    domain(index);

  override def remove(index: Int) {
    assert(present(index));
    _size -= 1;
    bvDomain.clear(index);
    removed = true
  }

  override def removeFrom(lb: Int) = {
    val nbRemVals = bvDomain.clearFrom(lb);
    removed = nbRemVals > 0
    _size -= nbRemVals;
    nbRemVals;
  }

  override def removeTo(ub: Int) = {
    val nbRemVals = bvDomain.clearTo(ub + 1);
    removed = nbRemVals > 0
    _size -= nbRemVals;
    nbRemVals;
  }

  override def getBitVector = bvDomain

  override val maxSize = domain.length

  override def setLevel(level: Int) {
    assert(level > currentLevel, "Given level " + level
      + " should be greater than current " + currentLevel)
    if (removed) {
      history ::= (currentLevel, bvDomain.clone, size)
      removed = false
    }
    currentLevel = level;

  }

  override def restoreLevel(level: Int) {
    assert(level < currentLevel);

    history = history.dropWhile(_._1 > level)
    if (history == Nil) {
      bvDomain.fill(true)
      _size = domain.length
    } else {
      history.head._2.copyTo(bvDomain)
      _size = history.head._3
    }
    currentLevel = level;
    //    _last = bvDomain.prevSetBit(domain.length);

  }

  override def getAtLevel(level: Int) = {
    if (level < currentLevel) {
      history.find(_._1 <= level) match {
        case Some(e) => e._2
        case _ => BitVector.newBitVector(domain.length, true);
      }
    } else bvDomain;
  }

  override val allValues = domain.toArray;

  override def currentValues = currentIndexes map value

  def currentIndexes = {
    val indexes = new Array[Int](size)
    var j = 0
    var i = firstIndex
    while (i != -1) {
      indexes(j) = i
      i = next(i)
      j += 1
    }
    indexes
  }

  override def toString = {
    if (size <= BitVectorDomain.DISPLAYED_VALUES) {
      mkString("[", ", ", "]");
    } else {
      take(BitVectorDomain.DISPLAYED_VALUES).mkString("[", ", ", " (" + (size - BitVectorDomain.DISPLAYED_VALUES) + " more)]")
    }
  }
}