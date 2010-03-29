package cspfj.util;

public final class SmallBitVector extends BitVector {

    private long word;

    public SmallBitVector(final int size, final boolean fill) {
        super(size);
        if (fill) {
            word = MASK >>> -size;
        } else {
            word = 0;
        }
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
        if (start >= Long.SIZE) {
            return -1;
        }
        final long next = word & (MASK << start);
        return next == 0 ? -1 : Long.numberOfTrailingZeros(next);
    }

    public void setSingle(final int index) {
        word = 0;
        set(index);
    }

    public void clearFrom(final int from) {
        word &= ~(MASK << from);
    }

    public boolean equals(final Object o) {
        return ((SmallBitVector) o).word == word;
    }

    public int hashCode() {
        return (int) (word ^ (word >>> 32));
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
        return (((SmallBitVector) bv).word & word) != 0;
    }

    public int intersects(final BitVector bv) {
        return intersects(bv, 0) ? 0 : -1;
    }

    public int realSize() {
        return 1;
    }

    public BitVector xor(final BitVector bv) {
        final SmallBitVector bitVector = new SmallBitVector(size, false);
        bitVector.word = (((SmallBitVector) bv).word ^ this.word)
                & (MASK >>> -size);
        return bitVector;
    }

    public BitVector and(final BitVector bv) {
        final SmallBitVector bitVector = new SmallBitVector(size, false);
        bitVector.word = (((SmallBitVector) bv).word & this.word)
                & (MASK >>> -size);
        return bitVector;
    }

    public BitVector inverse() {
        final SmallBitVector bitVector = new SmallBitVector(size, false);
        bitVector.word = ~this.word & (MASK >>> -size);
        return bitVector;
    }

    public void setAllBut(final int index) {
        word |= ~(1L << index) & (MASK >>> -size);
    }

    @Override
    public boolean isEmpty() {
        return word == 0L;
    }

    @Override
    public int cardinality() {
        return Long.bitCount(word);
    }

}
