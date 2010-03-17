package cspfj.problem;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

import cspfj.util.BitVector;

public class BitVectorDomain implements Domain {

    private final static int HISTORY_INCREMENT = 20;

    private BitVector bvDomain;

    private int[] domain;

    private int size;

    private BitVector[] bvHistory;
    private int[] dsHistory;

    private int last;

    public BitVectorDomain(int... domain) {
        bvDomain = BitVector.factory(domain.length, true);
        this.domain = domain.clone();
        size = domain.length;
        last = domain.length - 1;
        bvHistory = new BitVector[HISTORY_INCREMENT];
        for (int i = HISTORY_INCREMENT; --i >= 0;) {
            bvHistory[i] = BitVector.factory(domain.length, true);
        }
        dsHistory = new int[HISTORY_INCREMENT];
    }

    @Override
    public int first() {
        return bvDomain.nextSetBit(0);
    }

    @Override
    public int size() {
        return size;
    }

    @Override
    public int last() {
        assert last == bvDomain.prevSetBit(domain.length);
        return last;
    }

    @Override
    public int lastAbsent() {
        return bvDomain.prevClearBit(domain.length);
    }

    @Override
    public int next(int i) {
        return bvDomain.nextSetBit(i + 1);
    }

    @Override
    public int prev(int i) {
        return bvDomain.prevSetBit(i);
    }

    @Override
    public int prevAbsent(int i) {
        return bvDomain.prevClearBit(i);
    }

    /**
     * @param value
     *            the value we seek the index for
     * @return the index of the given value or -1 if it could not be found
     */
    public int index(final int value) {
        final int[] domain = this.domain;
        for (int i = domain.length; --i >= 0;) {
            if (domain[i] == value) {
                return i;
            }
        }

        return -1;
    }

    /**
     * @param index
     *            index to test
     * @return true iff index is present
     */
    @Override
    public boolean present(int index) {
        return bvDomain.get(index);
    }

    @Override
    public void setSingle(int index) {
        bvDomain.setSingle(index);
        size = 1;
        last = index;
    }

    @Override
    public int value(int index) {
        return domain[index];
    }

    @Override
    public void remove(int index) {
        size--;
        bvDomain.clear(index);
        if (index == last) {
            last = bvDomain.prevSetBit(domain.length);
        }
    }

    public BitVector getBitVector() {
        return bvDomain;
    }

    public BitVectorDomain clone() {
        final BitVectorDomain clone;
        try {
            clone = (BitVectorDomain) super.clone();
        } catch (CloneNotSupportedException e) {
            throw new IllegalStateException(e);
        }
        clone.domain = domain.clone();
        clone.bvHistory = bvHistory.clone();
        clone.dsHistory = dsHistory.clone();
        return clone;
    }

    @Override
    public int maxSize() {
        return domain.length;
    }

    private int currentLevel = 0;

    public void setLevel(int level) {
        assert level > currentLevel;
        if (currentLevel >= bvHistory.length) {
            bvHistory = Arrays.copyOf(bvHistory, currentLevel
                    + HISTORY_INCREMENT);
            dsHistory = Arrays.copyOf(dsHistory, currentLevel
                    + HISTORY_INCREMENT);
            for (int i = HISTORY_INCREMENT; --i >= 0;) {
                bvHistory[currentLevel + i] = BitVector.factory(domain.length,
                        false);
            }
        }

        bvDomain.copyTo(bvHistory[currentLevel]);
        dsHistory[currentLevel] = size;
        currentLevel = level;
    }

    public void restoreLevel(int level) {
        if (level < currentLevel) {
            bvHistory[level].copyTo(bvDomain);
            size = dsHistory[level];
            currentLevel = level;
            last = bvDomain.prevSetBit(domain.length);
        }
    }

    public BitVector getAtLevel(int level) {
        if (level < currentLevel) {
            return bvHistory[level];
        }

        return bvDomain;
    }

    @Override
    public int[] allValues() {
        return domain;
    }

    private boolean checkedOrdered = false;
    private boolean ordered;

    @Override
    public boolean isOrdered() {
        if (checkedOrdered) {
            return ordered;
        }
        checkedOrdered = true;
        ordered = true;
        for (int i = domain.length - 1; --i >= 0;) {
            if (domain[i + 1] < domain[i]) {
                ordered = false;
                break;
            }
        }
        return ordered;
    }
}
