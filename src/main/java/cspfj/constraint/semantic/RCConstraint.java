package cspfj.constraint.semantic;

import java.util.Arrays;
import java.util.BitSet;

import cspfj.constraint.AbstractConstraint;
import cspfj.constraint.DynamicConstraint;
import cspfj.filter.RevisionHandler;
import cspfj.problem.Variable;

public class RCConstraint extends AbstractConstraint implements
        DynamicConstraint {

    private final Interval[] intervals;

    private final Interval[] sortedIntervals;

    private final int[] position;

    // private final BitSet v1Validated;

    public RCConstraint(Variable[] scope) {
        super(scope);

        intervals = initIntervals(scope);
        sortedIntervals = intervals.clone();
        position = new int[intervals.length];

        //init(scope);
    }

    public RCConstraint(Variable[] scope, String name) {
        super(name, scope);

        intervals = initIntervals(scope);
        sortedIntervals = intervals.clone();
        position = new int[intervals.length];

        //init(scope);
    }

//    private void init(Variable[] scope) {
//        nbInitConflicts = new long[getArity()][];
//        nbMaxConflicts = new long[getArity()];
//        for (int i = getArity(); --i >= 0;) {
//            nbInitConflicts[i] = new long[scope[i].getDomain().maxSize()];
//        }
//        sortIntervals();
//    }

    private static Interval[] initIntervals(final Variable[] scope) {
        final Interval[] intervals = new Interval[scope[0].getDomain()
                .getAtLevel(0).lastSetBit() + 1];
        final int ub = scope[1].getDomain().getAtLevel(0).lastSetBit();
        for (int i = intervals.length; --i >= 0;) {
            intervals[i] = new Interval(0, ub, i);
        }
        return intervals;
    }

    @Override
    public boolean removeTuple(int[] tuple) {
        // final Collection<Integer> removed = intervals[tuple[0]]
        // .remove(tuple[1]);
        //
        // if (removed != null) {
        // Arrays.sort(sortedIntervals);
        //
        // for (int i : removed) {
        // tuple[1] = i;
        // addConflict(tuple);
        // }
        //
        // return removed.size();
        // }
        // return 0;

        if (intervals[tuple[0]].remove(tuple[1])) {
            sortIntervals();

            //addConflict(tuple);

            return true;
        }
        return false;
    }

    @Override
    public int removeTuples(int[] base) {
        if (removeTuple(base)) {
            return 1;
        }
        return 0;
    }

    @Override
    public boolean check() {
        return intervals[tuple[0]].in(tuple[1]);
    }

    @Override
    public boolean revise(RevisionHandler revisator, int reviseCount) {

        final Variable v0 = getVariable(0);
        final Variable v1 = getVariable(1);

        // Support condition
//        if (v1.getDomainSize() > nbMaxConflicts[0]
//                && v0.getDomainSize() > nbMaxConflicts[1]) {
//            return true;
//        }
        final BitSet v1Validated = new BitSet(v1.getLast() + 1);

        int itv = 0;

        final BitSet v0Present = new BitSet(v0.getLast() + 1);
        for (int i = v0.getFirst(); i >= 0; i = v0.getNext(i)) {
            v0Present.set(position[i]);
        }

        itv = v0Present.nextSetBit(0);

        boolean revised = false;

        for (int i = v1.getFirst(); i >= 0 && itv >= 0; i = v1.getNext(i)) {
            for (; itv >= 0 && sortedIntervals[itv].lb <= i; itv = v0Present
                    .nextSetBit(itv + 1)) {
                final Interval currentItv = sortedIntervals[itv];

                if (i <= currentItv.ub) {
                    v1Validated.set(currentItv.lb, currentItv.ub + 1);
                } else {
                    v0.remove(currentItv.index0);
                    if (v0.getDomainSize() == 0) {
                        return false;
                    }
                    revised = true;
                }

            }
        }

        if (v1Validated.isEmpty()) {
            return false;
        }

        if (revised) {
            revisator.revised(this, v0);
        }

        revised = false;
        final int end = v1.getLast();
        for (int i = v1.getFirst(); i >= 0 && i <= end;) {
            if (v1Validated.get(i)) {
                i = v1Validated.nextClearBit(i);
            } else {
                if (v1.isPresent(i)) {
                    v1.remove(i);
                    if (v1.getDomainSize() == 0) {
                        return false;
                    }
                    revised = true;
                }
                i = v1.getNext(i);
            }
        }

        if (revised) {
            revisator.revised(this, v1);
        }

        return true;
    }

    private void sortIntervals() {
        Arrays.sort(sortedIntervals);
        for (int i = position.length; --i >= 0;) {
            position[i] = Arrays.binarySearch(sortedIntervals, intervals[i]);
        }
    }

    public String toString() {
        int ub = 0;
        for (Interval i : intervals) {
            ub = Math.max(ub, i.ub);
        }
        return super.toString() + "[" + intervals.length + "," + ub + "]";
    }

    // public void flushPending() {
    // for (Interval i : intervals) {
    // i.flushPending();
    // }
    // }

    // private static class UnionInterval {
    //
    // private LinkedList<SimpleInterval> intervals;
    //
    // public UnionInterval() {
    // intervals = new LinkedList<SimpleInterval>();
    // }
    //
    // public void add(int lb, int ub) {
    // ListIterator<SimpleInterval> itr = intervals.listIterator();
    // while (itr.hasNext() && itr.next().lb < lb)
    // ;
    // itr.add(new SimpleInterval(lb, ub));
    // }
    //
    // public String toString() {
    // return intervals.toString();
    // }
    //
    // private static class SimpleInterval implements
    // Comparable<SimpleInterval> {
    // private int lb, ub;
    //
    // public SimpleInterval(int lb, int ub) {
    // this.lb = lb;
    // this.ub = ub;
    // }
    //
    // @Override
    // public int compareTo(SimpleInterval arg0) {
    // return lb - arg0.lb;
    // }
    //
    // public String toString() {
    // return "[" + lb + ", " + ub + "]";
    // }
    // }
    //
    // }
    //
    // public static void main(String args[]) {
    // UnionInterval itv = new UnionInterval();
    // System.out.println(itv);
    // itv.add(10, 15);
    // itv.add(1, 5);
    // itv.add(100, 150);
    // System.out.println(itv);
    // }

    // private static boolean intersect(Variable v, BitSet validated, int level)
    // {
    // boolean changed = false;
    //
    // int i = v.getFirst();
    //
    // while (i >= 0) {
    //
    // final int j = validated.nextClearBit(i);
    // if (i == j && v.isPresent(j)) {
    // v.remove(i, level);
    // changed = true;
    // } else if (j >= v.getDomain().length) {
    // break;
    // } else if (v.isPresent(j)) {
    // v.remove(j, level);
    // changed = true;
    // }
    // i = v.getNext(j);
    //
    // }
    // return changed;
    // }

    // private static boolean intersect(Variable v, BitSet validated, int level)
    // {
    // boolean changed = false;
    // final int end = v.getLast();
    // for (int i = v.getFirst(); i >= 0 && i <= end;) {
    // if (validated.get(i)) {
    // i = validated.nextClearBit(i);
    // } else {
    // if (v.isPresent(i)) {
    // v.remove(i, level);
    // changed = true;
    // }
    // i = v.getNextPresent(i);
    // }
    // }
    // return changed;
    // }

    // private static boolean intersect(Variable v, BitSet validated, int level)
    // {
    // boolean changed = false;
    // for (int i = v.getFirst(); i >= 0; i = v.getNext(i)) {
    // if (!validated.get(i)) {
    //
    // v.remove(i, level);
    // changed = true;
    //
    // }
    // }
    // return changed;
    // }

    // private static boolean intersect(Variable v, BitSet validated, int level)
    // {
    // boolean changed = false;
    // final int end = v.getLast();
    // for (int i = validated.nextClearBit(0); i <= end; i = validated
    // .nextClearBit(i + 1)) {
    // if (v.isPresent(i)) {
    // v.remove(i, level);
    // changed = true;
    // }
    // }
    // return changed;
    // }

    // private static class Interval implements Comparable<Interval> {
    // private int lb;
    // private int ub;
    // private final int index0;
    // private SortedSet<Integer> pending;
    // private static final Collection<Integer> removed = new
    // ArrayList<Integer>();
    //
    // public Interval(int lb, int ub, int index0) {
    // this.lb = lb;
    // this.ub = ub;
    // this.index0 = index0;
    // }
    //
    // public void flushPending() {
    // pending = null;
    // }
    //
    // public void incLb() {
    // removed.add(lb++);
    //
    // if (pending != null) {
    // while (!pending.isEmpty() && pending.first() < lb) {
    // pending.remove(pending.first());
    // }
    // while (!pending.isEmpty() && pending.first() == lb) {
    // removed.add(lb++);
    // pending.remove(pending.first());
    // }
    // }
    // }
    //
    // public void decUb() {
    // removed.add(ub--);
    //
    // if (pending != null) {
    // while (!pending.isEmpty() && pending.last() > ub) {
    // pending.remove(pending.last());
    // }
    // while (!pending.isEmpty() && pending.last() == ub) {
    // removed.add(ub--);
    // pending.remove(pending.last());
    // }
    // }
    // }
    //
    // public Collection<Integer> remove(final int value) {
    // removed.clear();
    // if (value == lb) {
    //
    // incLb();
    //
    // } else if (value == ub) {
    //
    // decUb();
    //
    // } else if (lb < value && value < ub) {
    //
    // if (pending == null) {
    // pending = new TreeSet<Integer>();
    // }
    // pending.add(value);
    //
    // }
    // return removed;
    // }
    //
    // public boolean in(final int value) {
    // return lb <= value && value <= ub;
    // }
    //
    // public String toString() {
    // return index0 + ": [" + lb + ", " + ub + "]";
    // }
    //
    // @Override
    // public int compareTo(Interval arg0) {
    // return lb - arg0.lb;
    // }
    // }
    private static class Interval implements Comparable<Interval> {
        private int lb;
        private int ub;
        private final int index0;

        // private static final Collection<Integer> removed = new
        // ArrayList<Integer>();

        public Interval(int lb, int ub, int index0) {
            this.lb = lb;
            this.ub = ub;
            this.index0 = index0;
        }

        public boolean remove(final int value) {
            if (value == lb) {
                lb++;
                return true;
            }
            if (value == ub) {
                ub--;
                return true;
            }
            return false;
        }

        public boolean in(final int value) {
            return lb <= value && value <= ub;
        }

        public String toString() {
            return index0 + ": [" + lb + ", " + ub + "]";
        }

        @Override
        public int compareTo(Interval arg0) {
            int c = lb - arg0.lb;
            if (c == 0) {
                return index0 - arg0.index0;
            }
            return c;
        }
    }
}
