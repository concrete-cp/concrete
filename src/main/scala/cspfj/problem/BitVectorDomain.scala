package cspfj.problem;

import cspfj.util.BitVector;

final class BitVectorDomain(
  val domain: Array[Int]) extends Domain with Cloneable {
  require(domain.sliding(2).forall(p => p(1) < p(2)), "Only ordered domains are supported");

  val HISTORY_INCREMENT = 20;
  val DISPLAYED_VALUES = 3;

  val indices = domain map { i => domain(i) -> i } toMap

  private var bvDomain = BitVector.newBitVector(domain.length, true)

  override var size = domain.size

  private var bvHistory = new Array[BitVector](HISTORY_INCREMENT)
  private var dsHistory = Array[Int](HISTORY_INCREMENT)

  private var _last = domain.size - 1

  private var currentLevel = 0;

  def this(domain: Int*) = this(domain.toArray)

  override def first() = bvDomain.nextSetBit(0);

  override def last() = {
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
    var lb = first();
    if (domain(lb) > value) {
      -1;
    } else {
      greatest(value, lb, last());
    }
  }
}
//
//    override def lowest(value: Int) = {
//        var ub = last();
//        if (domain(ub) < value) {
//             -1;
//        } else {
//        var lb = first();
//        while (domain(lb) < value) {
//            var test = (ub + lb) / 2;
//            if (domain(test) >= value) {
//                ub = test - 1;
//            } else {
//                lb = test + 1;
//            }
//        }
//        lb;
//        }
//    }
//
//    /**
//     * @param index
//     *            index to test
//     * @return true iff index is present
//     */
//    override def present(index: Int) = bvDomain.get(index);
//    
//
//    
//    override def setSingle(index: Int) {
//        bvDomain.setSingle(index);
//        size = 1;
//        _last = index;
//        if (bvHistory(currentLevel) == null) {
//            bvHistory(currentLevel) = BitVector.newBitVector(domain.length,
//                    false);
//        }
//    }
//
//    override def value(index: Int) {
//        domain(index);
//    }
//
//    override def remove(index: Int) {
//        assert (present(index));
//        size-=1;
//        bvDomain.clear(index);
//        if (index == _last) {
//            _last = bvDomain.prevSetBit(index);
//        }
//        assert (_last == bvDomain.prevSetBit(domain.length) , "Recorded " + last
//                + ", should be " + bvDomain.prevSetBit(domain.length)
//                + " given current domain " + toString());
//
//        if (bvHistory(currentLevel) == null) {
//            bvHistory(currentLevel) = BitVector.newBitVector(domain.length,
//                    false);
//        }
//    }
//
//    override def removeFrom(lb: Int ) = {
//        val removed = bvDomain.clearFrom(lb);
//        if (removed > 0) {
//            _last = bvDomain.prevSetBit(lb);
//            if (bvHistory(currentLevel) == null) {
//                bvHistory(currentLevel) = BitVector.newBitVector(domain.length,
//                        false);
//            }
//        }
//        size -= removed;
//        removed;
//    }
//
//    override def removeTo(ub: Int) = {
//        val removed = bvDomain.clearTo(ub + 1);
//        if (removed > 0 && bvHistory(currentLevel) == null) {
//            bvHistory(currentLevel) = BitVector.newBitVector(domain.length,
//                    false);
//        }
//        size -= removed;
//        removed;
//    }
//
//    override def getBitVector() = bvDomain
//
//    override def BitVectorDomain copy() = 
//        try {
//            clone()
//        } catch {
//          case e=>            throw new IllegalStateException(e);
//        }
//    }
//
//    public BitVectorDomain clone() throws CloneNotSupportedException {
//        final BitVectorDomain clone = (BitVectorDomain) super.clone();
//        clone.domain = domain.clone();
//        clone.bvHistory = bvHistory.clone();
//        clone.dsHistory = dsHistory.clone();
//        return clone;
//    }
//
//    @Override
//    public int maxSize() {
//        return domain.length;
//    }
//
//    @Override
//    public void setLevel(final int level) {
//        assert level > currentLevel : "Given level " + level
//                + " should be greater than current " + currentLevel;
//        ensureCapacity(level);
//        if (bvHistory[currentLevel] != null) {
//            bvDomain.copyTo(bvHistory[currentLevel]);
//            dsHistory[currentLevel] = size;
//        }
//        currentLevel = level;
//
//    }
//
//    private void ensureCapacity(final int size) {
//        if (size >= bvHistory.length) {
//            bvHistory = Arrays.copyOf(bvHistory, size + HISTORY_INCREMENT);
//            dsHistory = Arrays.copyOf(dsHistory, size + HISTORY_INCREMENT);
//        }
//    }
//
//    @Override
//    public void restoreLevel(final int level) {
//        assert level < currentLevel;
//        boolean change = false;
//        for (int l = currentLevel; l > level; l--) {
//            if (bvHistory[l] != null) {
//                change = true;
//                bvHistory[l] = null;
//            }
//        }
//        if (change) {
//            for (int l = level;; l--) {
//                if (l < 0) {
//                    bvDomain.fill(true);
//                    size = domain.length;
//                    break;
//                }
//                if (bvHistory[l] != null) {
//                    assert !bvHistory[l].isEmpty();
//                    bvHistory[l].copyTo(bvDomain);
//                    size = dsHistory[l];
//                    break;
//                }
//            }
//
//        }
//        currentLevel = level;
//        last = bvDomain.prevSetBit(domain.length);
//
//    }
//
//    @Override
//    public BitVector getAtLevel(final int level) {
//        if (level < currentLevel) {
//            for (int l = level; --l >= 0;) {
//                if (bvHistory[l] != null) {
//                    return bvHistory[l];
//                }
//            }
//            return BitVector.newBitVector(domain.length, true);
//        }
//
//        return bvDomain;
//    }
//
//    @Override
//    public int[] allValues() {
//        return domain;
//    }
//
//    @Override
//    public int[] currentValues() {
//        final int[] values = new int[size];
//        int j = 0;
//        for (int i = first(); i != -1; i = next(i)) {
//            values[j++] = value(i);
//        }
//        return values;
//    }
//
//    @Override
//    public String toString() {
//        final int first = first();
//        if (first < 0) {
//            return "[]";
//        }
//        final StringBuilder stb = new StringBuilder();
//
//        stb.append('[');
//
//        final int hideTo = size - DISPLAYED_VALUES;
//
//        for (int i = first, max = 0;;) {
//            if (++max <= DISPLAYED_VALUES || max > hideTo) {
//                stb.append(value(i));
//            } else if (max == hideTo) {
//                stb.append("(").append(size() - DISPLAYED_VALUES * 2)
//                        .append(" more)");
//
//            }
//            i = next(i);
//            if (i < 0) {
//                return stb.append(']').toString();
//            }
//            if (max <= DISPLAYED_VALUES || max >= hideTo) {
//                stb.append(", ");
//            }
//        }
//
//    }
//}
