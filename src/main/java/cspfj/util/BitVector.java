package cspfj.util;

import cspfj.problem.IntIterator;

public abstract class BitVector implements Cloneable {
	protected final static int ADDRESS_BITS_PER_WORD = 6;

	// Taille d'un long (64=2^6)
	protected final static int SIZE = 1 << ADDRESS_BITS_PER_WORD;

	protected final static long MASK = 0xFFFFFFFFFFFFFFFFL;

	protected final int size;
	
	protected BitVector(int size) {
		this.size = size;
	}

	public boolean set(final int position, final boolean status) {
		return status ? set(position) : clear(position);
	}

	public String toString() {
		final StringBuilder sb = new StringBuilder();
		sb.append('{');
		int i = nextSetBit(0);
		if (i != -1) {
			sb.append(i);
		}
		for (i = nextSetBit(i + 1); i != -1; i = nextSetBit(i + 1)) {
			sb.append(", ").append(i);
		}
		return sb.append('}').toString();

	}

	public static BitVector factory(int size, boolean fill) {
		return size > SIZE ? new LargeBitVector(size, fill)
				: new SmallBitVector(size, fill);
	}

	public BitVector clone() {
		try {
			return (BitVector) super.clone();
		} catch (CloneNotSupportedException e) {
			throw new IllegalStateException();
		}
	}

	public abstract void fill(final boolean fill);

	public abstract boolean clear(final int position);

	public abstract boolean set(final int position);

	public abstract boolean get(final int position);

	public abstract int nextSetBit(final int start);

	public abstract int prevSetBit(final int start);

	public abstract int prevClearBit(final int start);
	
	public int lastClearBit() {
		return prevClearBit(size);
	}

	public abstract void setSingle(final int index);

	public abstract void clearFrom(final int from);

	public abstract IntIterator iterator();

	public abstract void copyTo(BitVector bV);

	public abstract boolean intersects(BitVector bV, final int position);

	public abstract int intersects(BitVector bV);

	public abstract int realSize();
	
	public abstract BitVector exclusive(BitVector bv);
}
