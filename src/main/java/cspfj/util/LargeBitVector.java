package cspfj.util;

import java.util.Arrays;

final class LargeBitVector extends BitVector {
    private long[] words;

    public LargeBitVector(final int size, final boolean fill) {
        super(size);
        words = new long[nbWords(size)];
        fill(fill);
    }

    public LargeBitVector(final LargeBitVector bv) {
        super(bv.size);
        this.words = bv.words.clone();
    }

    public static int word(final int bit) {
        return bit >> ADDRESS_BITS_PER_WORD;
    }

    public void fill(final boolean fill) {
        if (fill) {
            Arrays.fill(words, MASK);
            words[words.length - 1] >>>= -size;
        } else {
            Arrays.fill(words, 0);
        }

    }

    public static int nbWords(final int nbBits) {
        if (nbBits % WORD_SIZE > 0) {
            return word(nbBits) + 1;
        }
        return word(nbBits);
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
        return (position * WORD_SIZE) + Long.numberOfTrailingZeros(word);
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
        return (1 + position) * WORD_SIZE - Long.numberOfLeadingZeros(word) - 1;
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
        return (1 + position) * WORD_SIZE - Long.numberOfLeadingZeros(word) - 1;
    }

    @Override
    public BitVector xor(final BitVector bv) {
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
    public BitVector and(final BitVector bv) {
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

    @Override
    public void setSingle(final int index) {
        Arrays.fill(words, 0);
        set(index);
    }

    @Override
    public int clearFrom(final int from) {
        int startWordIndex = word(from);
        int removed = Long.bitCount(words[startWordIndex]);
        // Handle first word
        words[startWordIndex] &= ~(MASK << from);
        removed -= Long.bitCount(words[startWordIndex]);
        // Handle intermediate words, if any
        for (int i = words.length; --i >= startWordIndex + 1;) {
            removed += Long.bitCount(words[i]);
            words[i] = 0;
        }
        return removed;
    }

    @Override
    public int clearTo(final int to) {
        if (to <= 0) {
            return 0;
        }
        int endWordIndex = word(to - 1);
        int removed = Long.bitCount(words[endWordIndex]);
        // Handle first word
        words[endWordIndex] &= ~(MASK >>> -to);
        removed -= Long.bitCount(words[endWordIndex]);
        // Handle intermediate words, if any
        for (int i = endWordIndex; --i >= 0;) {
            removed += Long.bitCount(words[i]);
            words[i] = 0;
        }
        return removed;
    }

    @Override
    public boolean equals(final Object o) {
        return Arrays.equals(words, ((LargeBitVector) o).words);
    }

    public int hashCode() {
        return Arrays.hashCode(words);
    }

    @Override
    public void copyTo(final BitVector bv) {
        System.arraycopy(words, 0, ((LargeBitVector) bv).words, 0, words.length);
    }

    @Override
    public boolean intersects(final BitVector bv, final int position) {
        return (((LargeBitVector) bv).words[position] & words[position]) != 0;
    }

    @Override
    public int intersects(final BitVector bv) {
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

    @Override
    public BitVector clone() throws CloneNotSupportedException {
        final LargeBitVector bv = (LargeBitVector) super.clone();
        bv.words = words.clone();
        return bv;
    }

    public boolean subsetOf(BitVector bv) {
        final long[] otherWords = ((LargeBitVector) bv).words;

        for (int i = words.length; --i >= 0;) {
            if ((words[i] & ~otherWords[i]) != 0L) {
                return false;
            }
        }
        return true;
    }
}
