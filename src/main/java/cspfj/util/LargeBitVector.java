package cspfj.util;

import java.util.Arrays;

public final class LargeBitVector extends BitVector {
    private final static int ADDRESS_BITS_PER_WORD = 6;

    // Taille d'un long (64=2^6)
    private final static int SIZE = 1 << ADDRESS_BITS_PER_WORD;

    private final static long MASK = 0xFFFFFFFFFFFFFFFFL;

    private long[] words;

    public LargeBitVector(final int size, final boolean fill) {
        super(size);
        words = new long[nbWords(size)];
        fill(fill);
    }

    public static int word(final int bit) {
        return bit >> ADDRESS_BITS_PER_WORD;
    }

    public void fill(final boolean fill) {
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

    public void setAllBut(final int index) {
        final int position = word(index);

        words[words.length - 1] = MASK >>> -size;
        for (int i = words.length - 1; --i >= 0;) {
            if (i == position) {
                words[i] |= ~(1L << position);
            } else {
                words[i] = MASK;
            }
        }

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

        final int wordsInUse = words.length;
        int position = Math.min(wordsInUse - 1, word(start));

        long word = words[position];
        if (position == word(start)) {
            word &= ~(MASK << start);
        }

        while (word == 0) {
            if (--position < 0) {
                return -1;
            }
            word = words[position];
        }
        return (1 + position) * SIZE - Long.numberOfLeadingZeros(word) - 1;
    }

    public int prevClearBit(final int start) {
        final int wordsInUse = words.length;
        int position = Math.min(wordsInUse - 1, word(start));

        long word = ~words[position];
        if (position == word(start)) {
            word &= ~(MASK << start);
        }

        while (word == 0) {
            if (--position < 0) {
                return -1;
            }
            word = ~words[position];
        }
        return (1 + position) * SIZE - Long.numberOfLeadingZeros(word) - 1;
    }

    
    @Override
    public BitVector xor(BitVector bv) {
        final LargeBitVector bitVector = new LargeBitVector(size, false);
        final LargeBitVector source = (LargeBitVector) bv;
        int i = words.length - 1;
        bitVector.words[i] = (source.words[i] & words[i]) ^ (MASK >>> -size);
        while (--i >= 0) {
            bitVector.words[i] = source.words[i] ^ words[i];
        }
        return bitVector;
    }

    
    @Override
    public BitVector and(BitVector bv) {
        final LargeBitVector bitVector = new LargeBitVector(size, false);
        final LargeBitVector source = (LargeBitVector) bv;
        int i = words.length - 1;
        bitVector.words[i] = (source.words[i] & words[i]) & (MASK >>> -size);
        while (--i >= 0) {
            bitVector.words[i] = source.words[i] & words[i];
        }
        return bitVector;
    }

    @Override
    public BitVector inverse() {
        final LargeBitVector bitVector = new LargeBitVector(size, false);
        int i = words.length - 1;
        bitVector.words[i] = ~words[i] & (MASK >>> -size);
        while (--i >= 0) {
            bitVector.words[i] = ~words[i];
        }
        return bitVector;
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

    public int realSize() {
        return words.length;
    }

    @Override
    public boolean isEmpty() {
        for (long l : words) {
            if (l != 0L) {
                return false;
            }
        }
        return true;
    }

    @Override
    public int cardinality() {
        int cardinality = 0;
        for (long w : words) {
            cardinality += Long.bitCount(w);
        }
        return cardinality;
    }

}
