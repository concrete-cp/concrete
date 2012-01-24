package cspfj.util;

public abstract class BitVector implements Cloneable {
    protected static final int ADDRESS_BITS_PER_WORD = 6;

    // Taille d'un long (64=2^6)
    protected static final int WORD_SIZE = 1 << ADDRESS_BITS_PER_WORD;

    protected static final long MASK = 0xFFFFFFFFFFFFFFFFL;

    protected final int size;

    public BitVector(final int size) {
        this.size = size;
    }

    public final boolean set(final int position, final boolean status) {
        if (status) {
            return set(position);
        }
        return clear(position);
    }

    @Override
    public final String toString() {
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

    public static BitVector newBitVector(final int size, final boolean fill) {
        if (size > WORD_SIZE) {
            return new LargeBitVector(size, fill);
        }
        return new SmallBitVector(size, fill);
    }

    public BitVector clone() throws CloneNotSupportedException {
        return (BitVector) super.clone();
    }

    public abstract void fill(final boolean fill);

    public abstract boolean clear(final int position);

    public abstract boolean set(final int position);

    public abstract boolean get(final int position);

    public abstract int nextSetBit(final int start);

    public abstract int prevSetBit(final int start);

    public final int lastSetBit() {
        return prevSetBit(size);
    }

    public abstract int prevClearBit(final int start);

    public final int lastClearBit() {
        return prevClearBit(size);
    }

    public abstract void setSingle(final int index);

    /**
     * Removes all values from given bound (included).
     * 
     * @param from
     * @return How many values were actually removed
     */
    public abstract int clearFrom(final int from);

    /**
     * Removes all values up to given bound (excluded).
     * 
     * @param ub
     * @return How many values were actually removed
     */
    public abstract int clearTo(int ub);

    public abstract void copyTo(BitVector bV);

    public abstract boolean intersects(BitVector bV, final int position);

    public abstract int intersects(BitVector bV);

    public abstract int realSize();

    public abstract BitVector xor(BitVector bv);

    public abstract BitVector and(BitVector bv);

    public abstract BitVector inverse();

    public abstract boolean isEmpty();

    public abstract void setAllBut(final int index);

    public abstract int cardinality();
    
    public abstract long getWord(int i);

    public boolean apply(int i) {
        return get(i);
    }

    public abstract boolean subsetOf(BitVector bv);

}
