package cspfj.util;

import java.util.Arrays;

import cspfj.problem.IntIterator;

public class LargeBitVector extends AbstractBitVector {
	private final static int ADDRESS_BITS_PER_WORD = 6;

	// Taille d'un long (64=2^6)
	private final static int SIZE = 1 << ADDRESS_BITS_PER_WORD;

	private final static long MASK = 0xFFFFFFFFFFFFFFFFL;

	private long[] words;

	public LargeBitVector(final int size, final boolean fill) {
		words = new long[nbWords(size)];
		fill(size, fill);
	}

	public static int word(final int bit) {
		return bit >> ADDRESS_BITS_PER_WORD;
	}

	public void fill(final int size, final boolean fill) {
		Arrays.fill(words, fill ? MASK : 0);
		words[words.length - 1] >>>= -size;
	}

	public static int nbWords(final int nbBits) {
		return (word(nbBits)) + (nbBits % SIZE > 0 ? 1 : 0);
	}

	public boolean clear(final int position) {
		final long old = words[word(position)];
		return (words[word(position)] &= ~(1L << position)) != old;
	}

	public boolean set(final int position) {
		final long old = words[word(position)];
		return (words[word(position)] |= 1L << position) != old;
	}

	public boolean get(final int position) {
		return (words[word(position)] & (1L << position)) != 0;
	}

	public int nextSetBit(final int start) {
		int position = word(start);
		final int wordsInUse = words.length;
		if (position >= wordsInUse) {
			return -1;
		}
		long word = words[position] & (MASK << start);

		while (word == 0) {
			if (++position == wordsInUse) {
				return -1;
			}
			word = words[position];
		}
		return (position * SIZE) + Long.numberOfTrailingZeros(word);
	}

	public int prevSetBit(final int start) {
		int position = word(start);
		final int wordsInUse = words.length;
		if (position >= wordsInUse) {
			return start;
		}

		long word = words[position] & ~(MASK << start);

		while (word == 0) {
			if (--position < 0) {
				return -1;
			}
			word = words[position];
		}
		return (1 + position) * SIZE - Long.numberOfLeadingZeros(word) - 1;
	}

	public int prevClearBit(final int start) {
		int position = word(start);
		final int wordsInUse = words.length;
		if (position >= wordsInUse) {
			return start;
		}

		long word = ~(words[position] | (MASK << start));

		while (word == 0) {
			if (--position < 0) {
				return -1;
			}
			word = ~words[position];
		}
		return (1 + position) * SIZE - Long.numberOfLeadingZeros(word) - 1;
	}

	public void setSingle(final int index) {
		Arrays.fill(words, 0);
		set(index);
	}

	public void clearFrom(int from) {
		int startWordIndex = word(from);

		// Handle first word
		words[startWordIndex] &= ~(MASK << from);

		// Handle intermediate words, if any
		Arrays.fill(words, startWordIndex + 1, words.length, 0);
	}

	public boolean equals(Object o) {
		return Arrays.equals(words, ((LargeBitVector) o).words);
	}

	public int hashCode() {
		return Arrays.hashCode(words);
	}

	public IntIterator iterator() {
		return new BVIterator();
	}

	public class BVIterator implements IntIterator {
		private int position;
		private long currentShift;
		private int current;

		public BVIterator() {
			position = 0;
			currentShift = words[0];
			while (currentShift == 0 && position < words.length - 1) {
				currentShift = words[++position];
			}

			if (currentShift == 0) {
				current = -1;
			} else {
				current = position * SIZE
						+ Long.numberOfTrailingZeros(words[0]);
				currentShift >>= current + 1;
			}
		}

		public boolean hasNext() {
			return current >= 0;
		}

		public int next() {
			final int ret = current;

			while (currentShift == 0 && position < words.length - 1) {
				currentShift = words[++position];
				current = position * SIZE - 1;
			}
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
		System
				.arraycopy(words, 0, ((LargeBitVector) bv).words, 0,
						words.length);
	}

	@Override
	public LargeBitVector clone() {
		final LargeBitVector bv = (LargeBitVector) super.clone();
		bv.words = words.clone();
		return bv;
	}

	@Override
	public boolean intersects(BitVector bv, int position) {
		return (((LargeBitVector) bv).words[position] & words[position]) != 0;
	}

	@Override
	public int intersects(BitVector bv) {
		for (int i = words.length; --i >= 0;) {
			if (intersects(bv, i)) {
				return i;
			}
		}

		return -1;
	}

}
