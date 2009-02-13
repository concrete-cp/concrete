package cspfj.util;

public abstract class AbstractBitVector implements BitVector {
	protected final static int ADDRESS_BITS_PER_WORD = 6;

	// Taille d'un long (64=2^6)
	protected final static int SIZE = 1 << ADDRESS_BITS_PER_WORD;

	protected final static long MASK = 0xFFFFFFFFFFFFFFFFL;

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
}
