package cspfj.util;


public class SmallBitVector extends BitVector {

	private long word;

	public SmallBitVector(final int size, final boolean fill) {
		super(size);
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

	@Override
	public void copyTo(BitVector bv) {
		((SmallBitVector) bv).word = word;
	}

	@Override
	public void fill(boolean fill) {
		word = fill ? (MASK >>> -size) : 0;
	}

	public boolean intersects(BitVector bv, int position) {
		return (((SmallBitVector) bv).word & word) != 0;
	}

	public int intersects(BitVector bv) {
		return intersects(bv, 0) ? 0 : -1;
	}
	
	public int realSize() {
		return 1;
	}
	
	public BitVector exclusive(BitVector bv) {
		final SmallBitVector bitVector = (SmallBitVector) bv.clone();
		bitVector.word = (bitVector.word ^ this.word) & (MASK >>> -size);
		return bitVector;
	}
}
