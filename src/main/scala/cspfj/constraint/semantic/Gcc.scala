package cspfj.constraint.semantic;

import java.util.ArrayDeque;
import java.util.Deque;
import java.util.HashSet;
import java.util.Set;

import com.google.common.collect.ObjectArrays;

import cspfj.constraint.AbstractConstraint;
import cspfj.filter.RevisionHandler;
import cspfj.problem.Variable;

public final class Gcc extends AbstractConstraint {

    private final Bounds[] bounds;

    private final Deque<Variable> queue;

    public Gcc(final Variable[] vars, final Bounds[] bounds) {
        super(vars);
        int max = bounds[0].value;
        for (int i = 1; i < bounds.length; i++) {
            max = Math.max(max, bounds[i].value);
        }
        this.bounds = new Bounds[max + 1];
        for (Bounds c : bounds) {
            this.bounds[c.value] = c;
        }

        for (Variable v : vars) {
            for (int i = v.getFirst(); i >= 0; i = v.getNext(i)) {
                final int value = v.getValue(i);
                if (value > this.bounds.length || this.bounds[value] == null) {
                    throw new IllegalArgumentException("Value " + value
                            + " is not bounded");
                }
            }
        }

        queue = new ArrayDeque<Variable>(vars.length);
    }

    public static final class Bounds {
        private int value;
        private int minCount;
        private int maxCount;

        public Bounds(final int value, final int minCount, final int maxCount) {
            this.value = value;
            this.minCount = minCount;
            this.maxCount = maxCount;
        }

        @Override
        public String toString() {
            return value + ": [" + minCount + ", " + maxCount + "]";
        }
    }

    @Override
    public boolean check() {
        final int[] counts = new int[bounds.length];
        for (int i = getArity(); --i >= 0;) {
            final int value = getValue(i);
            if (++counts[value] > bounds[value].maxCount) {
                return false;
            }
        }
        for (int i = counts.length; --i >= 0;) {
            final Bounds bds = bounds[i];
            if (bds == null && counts[i] > 0) {
                return false;
            }
            if (counts[i] > bds.minCount) {
                return false;
            }
        }
        return true;
    }

    private boolean filter(final Set<Variable> except, final int value,
            final RevisionHandler revisator) {
        for (Variable v : getScope()) {
            if (except.contains(v)) {
                continue;
            }
            final int index = v.getDomain().index(value);
            if (index >= 0 && v.isPresent(index)) {
                v.remove(index);
                if (v.getDomainSize() < 1) {
                    return true;
                } else if (v.getDomainSize() == 1) {
                    queue.offer(v);
                }
                revisator.revised(this, v);
            }

        }
        return false;
    }

    private void assignAll(final int value, final RevisionHandler revisator) {
        for (Variable v : getScope()) {
            final int index = v.getDomain().index(value);
            if (index >= 0 && v.isPresent(index)) {
                v.setSingle(index);
                revisator.revised(this, v);
            }
        }
    }

    @Override
    public boolean revise(final RevisionHandler revisator, final int reviseCount) {
        /*
         * Upper bounds
         */
        queue.clear();
        for (int pos = getArity(); --pos >= 0;) {
            if (getVariable(pos).getDomainSize() == 1) {
                queue.offer(getVariable(pos));
            }
        }

        @SuppressWarnings("unchecked")
        final Set<Variable>[] singles = ObjectArrays.newArray(Set.class,
                bounds.length);

        while (!queue.isEmpty()) {
            final Variable var = queue.poll();
            final int value = var.getValue(var.getFirst());

            final Set<Variable> singletons;

            if (singles[value] == null) {
                singletons = new HashSet<Variable>();
                singles[value] = singletons;
            } else {
                singletons = singles[value];
            }
            singletons.add(var);
            if (singletons.size() == bounds[value].maxCount) {
                if (filter(singletons, value, revisator)) {
                    return false;
                }
            } else if (singletons.size() > bounds[value].maxCount) {
                return false;
            }
        }

        /*
         * Lower bounds
         */
        final int[] counts = new int[bounds.length];
        for (Variable v : getScope()) {
            for (int i = v.getFirst(); i >= 0; i = v.getNext(i)) {
                counts[v.getValue(i)]++;
            }
        }

        for (Bounds b : bounds) {
            if (b == null) {
                continue;
            }
            if (counts[b.value] < b.minCount) {
                return false;
            }
            if (counts[b.value] == b.minCount) {
                assignAll(b.value, revisator);
            }
        }

        return true;
    }

    @Override
    public float getEvaluation() {
        return getArity() * getArity();
    }

}
