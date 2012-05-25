package cspfj.util;

public final class SmallBitVector extends BitVector {

    private long word;

    public SmallBitVector(final int size) {
        super(size);
        if (size > WORD_SIZE) {
            throw new IllegalArgumentException(
                    "Cannot handle vectors larger than " + WORD_SIZE
                            + " elements");
        }
    }

    public boolean clear(final int position) {
        assert position < size;
        final long old = word;
        word &= ~(1L << position);
        return word != old;
    }

    public boolean set(final int position) {
        assert position < size;
        final long old = word;
        word |= (1L << position);
        return word != old;
    }

    public boolean get(final int position) {
        if (position >= size)
            return false;

        return (word & (1L << position)) != 0;
    }

    public int prevSetBit(final int start) {

        final long prev;
        if (start >= WORD_SIZE) {
            prev = word;
        } else {
            prev = word & ~(MASK << start);
        }
        if (prev == 0) {
            return -1;
        }
        return WORD_SIZE - Long.numberOfLeadingZeros(prev) - 1;
    }

    public int prevClearBit(final int start) {
        final long prev;
        if (start >= WORD_SIZE) {
            prev = ~word;
        } else {
            prev = ~word & ~(MASK << start);
        }
        return prev == 0 ? -1
                : (WORD_SIZE - Long.numberOfLeadingZeros(prev) - 1);
    }

    public int nextSetBit(final int start) {
        if (start >= size) {
            return -1;
        }
        final long next = word & (MASK << start);
        return next == 0 ? -1 : Long.numberOfTrailingZeros(next);
    }

    public boolean clearFrom(final int from) {
        if (from >= size)
            return false;
        final long before = word;
        word &= ~(MASK << from);
        return word != before;
    }

    public boolean setFrom(final int from) {
        if (from >= size)
            return false;
        final long before = word;
        word |= MASK << from;
        word &= MASK >>> -size;
        return word != before;
    }

    public boolean clearTo(final int to) {
        if (to <= 0) {
            return false;
        }
        final long before = word;
        word &= ~(MASK >>> -to);
        return word != before;
    }

    public boolean equals(final Object o) {
        BitVector bv = (BitVector) o;
        return bv.getWord(0) == word && bv.nextSetBit(WORD_SIZE) == -1;
    }

    public int hashCode() {
        return (int) word;
    }

    @Override
    public void copyTo(final BitVector bv) {
        ((SmallBitVector) bv).word = word;
    }

    @Override
    public void fill(final boolean fill) {
        word = fill ? (MASK >>> -size) : 0;
    }

    public boolean intersects(final BitVector bv, final int position) {
        return (bv.getWord(0) & word) != 0;
    }

    public int intersects(final BitVector bv) {
        return intersects(bv, 0) ? 0 : -1;
    }

    public int realSize() {
        return 1;
    }

    public BitVector xor(final BitVector bv) {
        try {
            final BitVector bitVector = bv.clone();
            bitVector.setFirstWord(bv.getWord(0) ^ this.word);
            return bitVector;
        } catch (CloneNotSupportedException e) {
            throw new IllegalStateException(e);
        }

    }

    public BitVector and(final BitVector bv) {
        final SmallBitVector bitVector = new SmallBitVector(size);
        bitVector.word = (bv.getWord(0) & this.word) & (MASK >>> -size);
        return bitVector;
    }

    public BitVector inverse() {
        final SmallBitVector bitVector = new SmallBitVector(size);
        bitVector.word = ~this.word & (MASK >>> -size);
        return bitVector;
    }

    @Override
    public boolean isEmpty() {
        return word == 0L;
    }

    @Override
    public int cardinality() {
        return Long.bitCount(word);
    }

    @Override
    public boolean subsetOf(BitVector bv) {
        return (word & ~bv.getWord(0)) == 0L;
    }

    @Override
    public long getWord(int i) {
        if (i > 0)
            return 0L;
        else
            return word;
    }

    @Override
    protected void setFirstWord(long word) {
        this.word = word;

    }
}
