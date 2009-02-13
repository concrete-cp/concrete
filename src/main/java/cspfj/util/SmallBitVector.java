package cspfj.util;

import cspfj.problem.IntIterator;

public class SmallBitVector extends AbstractBitVector {

	private long word;

	public SmallBitVector(final int size, final boolean fill) {
		super();
		word = fill ? MASK >>> -size : 0;
	}

	public boolean clear(final int position) {
		final long old = word;
		return (word &= ~(1L << position)) != old;
	}

	public boolean set(final int position) {
		final long old = word;
		return (word |= (1L << position)) != old;
	}

	public boolean get(final int position) {
		return (word & (1L << position)) != 0;
	}

	public int prevSetBit(final int start) {
		final long prev = word & ~(MASK << start);
		return prev == 0 ? -1 : (SIZE - Long.numberOfLeadingZeros(prev) - 1);
	}

	public int prevClearBit(final int start) {
		final long prev = ~word & ~(MASK << start);
		return prev == 0 ? -1 : (SIZE - Long.numberOfLeadingZeros(prev) - 1);
	}

	public int nextSetBit(final int start) {
		final long next = word & (MASK << start);
		return next == 0 ? -1 : Long.numberOfTrailingZeros(next);
	}

	public void setSingle(final int index) {
		word = 0;
		set(index);
	}

	public void clearFrom(int from) {
		word &= ~(MASK << from);
	}

	public boolean equals(Object o) {
		return ((SmallBitVector) o).word == word;
	}

	public int hashCode() {
		return (int) (word ^ (word >>> 32));
	}

	public IntIterator iterator() {
		return new SBVIterator();
	}

	public class SBVIterator implements IntIterator {
		private long currentShift;
		private int current;

		public SBVIterator() {
			if (word == 0) {
				current = -1;
			} else {
				current = Long.numberOfTrailingZeros(word);
				currentShift = word >> (current + 1);
			}
		}

		public boolean hasNext() {
			return current >= 0;
		}

		public int next() {
			final int ret = current;
			if (currentShift == 0) {
				current = -1;
			} else {
				final int next = Long.numberOfTrailingZeros(currentShift) + 1;
				current += next;
				currentShift >>= next;
			}
			return ret;
		}

	}

	@Override
	public void copyTo(BitVector bv) {
		((SmallBitVector) bv).word = word;
	}

	@Override
	public void fill(int size, boolean fill) {
		word = fill ? (MASK >>> -size) : 0;
	}

	public boolean intersects(BitVector bv, int position) {
		return (((SmallBitVector) bv).word & word) != 0;
	}

	public int intersects(BitVector bv) {
		return intersects(bv, 0) ? 0 : -1;
	}
}
