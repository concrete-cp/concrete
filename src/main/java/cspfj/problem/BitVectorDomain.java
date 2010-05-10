package cspfj.problem;

import java.util.Arrays;

import cspfj.util.Arrays2;
import cspfj.util.BitVector;

public final class BitVectorDomain implements Domain {

	private static final int HISTORY_INCREMENT = 20;

	private static final int DISPLAYED_VALUES = 5;

	private BitVector bvDomain;

	private int[] domain;

	private int size;

	private BitVector[] bvHistory;
	private int[] dsHistory;

	private int last;

	private int currentLevel = 0;

	public BitVectorDomain(final int... domain) {
		if (!Arrays2.isOrdered(domain)) {
			throw new IllegalArgumentException(
					"Only ordered domains are supported");
		}
		bvDomain = BitVector.factory(domain.length, true);
		this.domain = domain.clone();
		size = domain.length;
		last = domain.length - 1;
		bvHistory = new BitVector[HISTORY_INCREMENT];
		dsHistory = new int[HISTORY_INCREMENT];

	}

	@Override
	public int first() {
		return bvDomain.nextSetBit(0);
	}

	@Override
	public int size() {
		return size;
	}

	@Override
	public int last() {
		assert last == bvDomain.prevSetBit(domain.length) : "Recorded " + last
				+ ", should be " + bvDomain.prevSetBit(domain.length)
				+ " given current domain " + toString();
		return last;
	}

	@Override
	public int lastAbsent() {
		return bvDomain.prevClearBit(domain.length);
	}

	@Override
	public int next(final int i) {
		return bvDomain.nextSetBit(i + 1);
	}

	@Override
	public int prev(final int i) {
		return bvDomain.prevSetBit(i);
	}

	@Override
	public int prevAbsent(final int i) {
		return bvDomain.prevClearBit(i);
	}

	/**
	 * @param value
	 *            the value we seek the index for
	 * @return the index of the given value or -1 if it could not be found
	 */
	public int index(final int value) {
		return Arrays.binarySearch(domain, value);
	}

	@Override
	public int greatest(final int value) {
		int lb = first();
		if (value(lb) > value) {
			return -1;
		}
		int ub = last();
		while (value(ub) > value) {
			int test = (ub + lb) / 2;
			if (value(test) > value) {
				ub = test - 1;
			} else {
				lb = test + 1;
			}
		}
		return ub;
	}

	@Override
	public int lowest(final int value) {
		int ub = last();
		if (value(ub) < value) {
			return -1;
		}
		int lb = first();
		while (value(lb) < value) {
			int test = (ub + lb) / 2;
			if (value(test) >= value) {
				ub = test - 1;
			} else {
				lb = test + 1;
			}
		}
		return lb;
	}

	/**
	 * @param index
	 *            index to test
	 * @return true iff index is present
	 */
	@Override
	public boolean present(final int index) {
		return bvDomain.get(index);
	}

	@Override
	public void setSingle(final int index) {
		bvDomain.setSingle(index);
		size = 1;
		last = index;
		if (bvHistory[currentLevel] == null) {
			bvHistory[currentLevel] = BitVector.factory(domain.length, false);
		}
	}

	@Override
	public int value(final int index) {
		return domain[index];
	}

	@Override
	public void remove(final int index) {
		assert present(index);
		size--;
		bvDomain.clear(index);
		if (index == last) {
			last = bvDomain.prevSetBit(index);
		}
		assert last == bvDomain.prevSetBit(domain.length) : "Recorded " + last
				+ ", should be " + bvDomain.prevSetBit(domain.length)
				+ " given current domain " + toString();

		if (bvHistory[currentLevel] == null) {
			bvHistory[currentLevel] = BitVector.factory(domain.length, false);
		}
	}

	@Override
	public int removeFrom(final int lb) {
		final int removed = bvDomain.clearFrom(lb);
		if (removed > 0) {
			last = bvDomain.prevSetBit(last + 1);
			if (bvHistory[currentLevel] == null) {
				bvHistory[currentLevel] = BitVector.factory(domain.length,
						false);
			}
		}
		size -= removed;
		return removed;
	}

	@Override
	public int removeTo(final int ub) {
		final int removed = bvDomain.clearTo(ub);
		if (removed > 0 && bvHistory[currentLevel] == null) {
			bvHistory[currentLevel] = BitVector.factory(domain.length, false);
		}
		size -= removed;
		return removed;
	}

	public BitVector getBitVector() {
		return bvDomain;
	}

	public BitVectorDomain clone() {
		final BitVectorDomain clone;
		try {
			clone = (BitVectorDomain) super.clone();
		} catch (CloneNotSupportedException e) {
			throw new IllegalStateException(e);
		}
		clone.domain = domain.clone();
		clone.bvHistory = bvHistory.clone();
		clone.dsHistory = dsHistory.clone();
		return clone;
	}

	@Override
	public int maxSize() {
		return domain.length;
	}

	@Override
	public void setLevel(final int level) {
		assert level > currentLevel : "Given level " + level
				+ " should be greater than current " + currentLevel;
		ensureCapacity(level);
		if (bvHistory[currentLevel] != null) {
			bvDomain.copyTo(bvHistory[currentLevel]);
			dsHistory[currentLevel] = size;
		}
		currentLevel = level;

	}

	private void ensureCapacity(final int size) {
		if (size >= bvHistory.length) {
			bvHistory = Arrays.copyOf(bvHistory, currentLevel
					+ HISTORY_INCREMENT);
			dsHistory = Arrays.copyOf(dsHistory, currentLevel
					+ HISTORY_INCREMENT);
		}
	}

	@Override
	public void restoreLevel(final int level) {
		assert level < currentLevel;
		boolean change = false;
		for (int l = currentLevel; l > level; l--) {
			if (bvHistory[l] != null) {
				change = true;
				bvHistory[l] = null;
			}
		}
		if (change) {
			for (int l = level;; l--) {
				if (l < 0) {
					bvDomain.fill(true);
					size = domain.length;
					break;
				}
				if (bvHistory[l] != null) {
					assert !bvHistory[l].isEmpty();
					bvHistory[l].copyTo(bvDomain);
					size = dsHistory[l];
					break;
				}
			}

		}
		currentLevel = level;
		last = bvDomain.prevSetBit(domain.length);

	}

	@Override
	public BitVector getAtLevel(final int level) {
		if (level < currentLevel) {
			for (int l = level; --l >= 0;) {
				if (bvHistory[l] != null) {
					return bvHistory[l];
				}
			}
			return BitVector.factory(domain.length, true);
		}

		return bvDomain;
	}

	@Override
	public int[] allValues() {
		return domain;
	}

	@Override
	public int[] currentValues() {
		final int[] values = new int[size];
		int j = 0;
		for (int i = first(); i != -1; i = next(i)) {
			values[j++] = value(i);
		}
		return values;
	}

	@Override
	public String toString() {
		final int first = first();
		if (first < 0) {
			return "[]";
		}
		final StringBuilder stb = new StringBuilder();

		stb.append('[');

		for (int i = first, max = DISPLAYED_VALUES;;) {
			stb.append(value(i));
			i = next(i);
			if (i < 0) {
				return stb.append(']').toString();
			}
			if (--max == 0) {
				return stb.append("... (").append(size() - DISPLAYED_VALUES)
						.append(" more)]").toString();
			}
			stb.append(", ");
		}
	}
}
