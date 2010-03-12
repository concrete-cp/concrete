package cspfj.constraint.extension;

import java.util.Arrays;
import java.util.Iterator;

import cspfj.problem.IntVariable;

public class MatrixManagerDynamic extends MatrixManager implements
        Iterable<int[]> {

    private TupleSet tupleSet;

    private int[][] tupleList;

    private int first;

    private int[] next;

    private int[] removed;

    private int[] removedLast;

    // private int stingRemoved;

    public MatrixManagerDynamic(final IntVariable[] scope, final TupleSet matrix,
            final boolean shared) {
        super(scope, matrix, shared);
        this.tupleSet = matrix;
        tupleList = new int[matrix.size()][];

        int i = 0;
        for (Iterator<int[]> itr = matrix.iterator(); itr.hasNext(); i++) {
            tupleList[i] = itr.next();
        }
        first = -1;
        next = new int[tupleSet.size()];
        Arrays.fill(next, -1);

        removed = new int[arity];
        Arrays.fill(removed, -1);
        removedLast = removed.clone();

        for (i = tupleList.length; --i >= 0;) {
            if (tupleList[i] != null && tupleList[i].length == scope.length) {
                addCell(i);
            }
        }

    }

    private void expandRemoved(final int newLength) {

        int oldLength = removed.length;
        removed = Arrays.copyOf(removed, newLength);
        Arrays.fill(removed, oldLength, removed.length, -1);

        removedLast = Arrays.copyOf(removedLast, newLength);
    }

    private int currentLevel;

    public void setLevel(int level) {
        currentLevel = level;
    }

    public void restore(final int level) {
        for (int i = removed.length; --i > level;) {
            if (removed[i] >= 0) {
                addAll(removed[i], removedLast[i]);
                removed[i] = -1;
            }
        }
        currentLevel = level;

    }

    private void addAll(final int index, final int last) {

        next[last] = first;
        first = index;

    }

    private void addCell(final int index) {
        next[index] = first;
        first = index;
    }

    @Override
    public LLIterator iterator() {
        return new LLIterator();
    }

    public Iterator<int[]> hsIterator() {
        return tupleSet.iterator();
    }

    public Matrix unshareMatrix() {
        return tupleSet = (TupleSet) super.unshareMatrix();
    }

    public MatrixManagerDynamic clone() throws CloneNotSupportedException {
        final MatrixManagerDynamic list = (MatrixManagerDynamic) super.clone();
        list.next = next.clone();
        // list.prev = prev.clone();
        list.removed = removed.clone();
        list.removedLast = removedLast.clone();
        return list;

    }

    public String toString() {
        final StringBuilder stb = new StringBuilder();

        for (int[] tuple : this) {
            stb.append(Arrays.toString(tuple)).append(",");
        }
        return stb.toString();
    }

    public boolean removeTuple(int[] tuple) {
        if (super.removeTuple(tuple)) {
            // stingRemoved = currentLevel;
            return true;
        }
        return false;
    }

    public class LLIterator implements Iterator<int[]> {

        private int current = -1;

        private int prev = -1;

        private int nextOne;

        public LLIterator() {
            nextOne = first;
        }

        @Override
        public boolean hasNext() {
            if (nextOne < 0) {
                return false;
            }
            return true;
        }

        @Override
        public int[] next() {

            prev = current;
            current = nextOne;

            if (current >= 0) {
                nextOne = next[current];
            }

            // if (!tupleSet.check(tupleList[current])) {
            // return next();
            // }

            return tupleList[current];
        }

        @Override
        public void remove() {
            remove(currentLevel);
        }

        public void remove(final int level) {
            if (prev < 0) {
                first = nextOne;
            } else {
                next[prev] = nextOne;
            }

            if (level >= 0) {

                // assert count() == count - 1 : count + "->" + count();

                if (level >= removed.length) {
                    expandRemoved(level + 1);
                }

                final int oldFirstRemoved = removed[level];

                next[current] = oldFirstRemoved;
                removed[level] = current;

                if (oldFirstRemoved < 0) {
                    removedLast[level] = current;
                }
            }
            current = prev;
        }

        public void promote() {
            if (prev < 0) {
                return;
            }

            next[prev] = nextOne;
            next[current] = first;
            first = current;

            current = prev;

        }

    }

    public int getSize() {
        return tupleSet.size();
    }
}
