package concrete.constraint.semantic.energy;

import java.util.ArrayList;
import java.util.Collection;

public class IntervalChain {
    // The upper part stores the lower bound and the down part the upper bounds
    private DoubleEndedStack stack;
    private int currentKey;

    public IntervalChain(int l, int u, int maxIndex, int value, int maxN) {
        stack = new DoubleEndedStack(maxN * 2);
        stack.pushL(l - 1, 0, value);
        stack.pushU(u + 1, maxIndex, value);
        currentKey = 0;
    }

    public Interval next() {
        if (currentKey >= stack.size() - 1) {
            currentKey = 0;
            return null;
        }
        return new Interval(stack.getKey(currentKey), stack.getKey(currentKey + 1), stack.getValue(currentKey++));
    }

    public Collection<Interval> getIntervals() {
        ArrayList<Interval> intervals = new ArrayList<>();
        for (int i = 0; i < stack.size() - 1; i++) {
            if (stack.getKey(i) != stack.getKey(i + 1)) {
                intervals.add(new Interval(stack.getKey(i), stack.getKey(i + 1), stack.getValue(i)));
            }
        }

        return intervals;
    }

    public void replace(int l, int u, int lIndex, int uIndex, int value) {
        if (l < stack.topL()) {
            throw new RuntimeException("WTF");
        }

        while (l >= stack.topU()) {
            stack.pushL(stack.topU(), stack.topUIndex(), stack.topUValue());
            stack.popU();
        }

        while (u >= stack.topU()) {
            stack.popU();
        }

        stack.pushL(l, lIndex, value);
        stack.pushU(u, uIndex, value);
    }

    public int findAssociatedLower(int u) {
        int index = stack.findGreastestIndexSmallerThan(u);
        return stack.getValue(index);
    }


    public Interval findAssociatedLowerInterval(int u) {
        int index = stack.findGreastestIndexSmallerThan(u);
        return createIntervalForIndex(index);
    }

    public Interval findNextEntry(int key) {
        int index = stack.findSmallestIndexGreaterThan(key);
        if (index == -1 || index >= stack.size() - 1) {
            return null;
        }

        return createIntervalForIndex(index);
    }

    public DoubleEndedStack getStack() {
        return stack;
    }

    private Interval createIntervalForIndex(int index) {
        return new Interval(stack.getKey(index), stack.getKey(index + 1), stack.getIndex(index), stack.getIndex(index + 1), stack.getValue(index));
    }

    public class Interval {
        public int l;
        public int u;
        public int value;
        public int lIndex;
        public int uIndex;

        public Interval(int l, int u, int value) {
            this.l = l;
            this.u = u;
            this.value = value;
        }

        public Interval(int l, int u, int lIndex, int uIndex, int value) {
            this.l = l;
            this.u = u;
            this.lIndex = lIndex;
            this.uIndex = uIndex;
            this.value = value;
        }
    }
}
