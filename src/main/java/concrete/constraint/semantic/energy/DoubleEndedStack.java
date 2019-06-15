package concrete.constraint.semantic.energy;

// The idea of this datastructure is to have two stacks in one.
// Since the algorithm that will use it needs one stack that will be ordered
// non-decreasingly and one stack that will be ordered non-increasingly, the idea
// is to put both stacks in one array that will be ordered non-decreasingly. It
// will allow to search both stacks at the same time instead of having to
// search both (and code both with different ordering).
//
// Since the stack is used mainly for the IntervalChain, one stack will be named L and the other U,
// that is one stack for the lower bound and one for the upper bound of the intervals.
public class DoubleEndedStack {
    private int[] keys;
    private int[] values;
    private int[] indexes;
    private int lSize;
    private int uSize;
    private int capacity;

    public DoubleEndedStack (int capacity) {
        this.capacity = capacity;
        this.keys = new int[capacity];
        this.values = new int[capacity];
        this.indexes = new int[capacity];
        this.lSize = 0;
        this.uSize = 0;
    }

    public void pushL(int key, int lIndex, int value) {
        assert size() < capacity;
        keys[lSize] = key;
        values[lSize] = value;
        indexes[lSize] = lIndex;
        lSize++;
    }

    public int topL() {
        return keys[lSize -1];
    }

    public int topLValue() {
        return values[lSize -1];
    }

    public int topLIndex() {
        return indexes[lSize -1];
    }

    public void popL() {
        assert lSize > 0;
        lSize--;
    }

    public void pushU(int key, int uIndex, int value) {
        assert size() < capacity;
        int index = capacity - uSize - 1;
        keys[index] = key;
        values[index] = value;
        indexes[index] = uIndex;
        uSize++;
    }

    public int topU() {
        return keys[capacity - uSize];
    }

    public int topUIndex() {
        return indexes[capacity - uSize];
    }

    public int topUValue() {
        return getValue(lSize);
    }

    public void popU() {
        assert uSize > 0;
        uSize--;
    }

    public int getKey(int index) {
        assert index < size();
        return keys[getArrayIndex(index)];
    }

    public int getValue(int index) {
        assert index < size();

        if (index < lSize || index == size() - 1) {
            return values[getArrayIndex(index)];
        }
        return values[getArrayIndex(index+1)];
    }

    public int getIndex(int index) {
        assert index < size();

        return indexes[getArrayIndex(index)];
    }

    public int getArrayIndex(int i) {
        assert i < size();
        if (i < lSize) {
            return i;
        }
        int firstDownIndex = capacity - uSize;
        return firstDownIndex + i - lSize;
    }

    public int findGreastestIndexSmallerThan(int bound) {
        int left = 0;
        int right = size();
        int greatest = -1;

        while (left < right) {
            int middle = (left + right) / 2;
            if (getKey(middle) >= bound) {
                right = middle;
            } else {
                greatest = middle;
                left = middle + 1;
            }
        }
        return greatest;
    }

    public int findSmallestIndexGreaterThan(int bound) {
        int left = 0;
        int right = size();
        int greatest = -1;

        while (left < right) {
            int middle = (left + right) / 2;
            if (getKey(middle) <= bound) {
                left = middle + 1;
            } else {
                greatest = middle;
                right = middle;
            }
        }
        return greatest;
    }

    public int size() {
        return lSize + uSize;
    }

    public int uSize() {
        return uSize;
    }

    public int lSize() {
        return lSize;
    }
}
