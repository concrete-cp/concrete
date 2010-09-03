/**
 * CSPFJ - CSP solving API for Java
 * Copyright (C) 2006 Julien VION
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
 */

package cspfj.constraint.semantic;

import java.util.Arrays;
import java.util.Deque;
import java.util.LinkedList;

import cspfj.constraint.AbstractArcGrainedConstraint;
import cspfj.filter.RevisionHandler;
import cspfj.problem.Variable;
import cspfj.util.BitVector;

public final class AllDifferent extends AbstractArcGrainedConstraint {

    private final BitVector union;

    private final Deque<Variable> queue;

    private int offset;

    private final float priority;

    // private final static Logger logger = Logger
    // .getLogger("cspfj.constraint.AllDifferentConstraint");
    public AllDifferent(final Variable... scope) {
        this(null, scope);
    }

    public AllDifferent(final String name, final Variable... scope) {
        super(name, scope);

        offset = Integer.MAX_VALUE;
        int max = Integer.MIN_VALUE;
        for (Variable v : scope) {
            for (int i : v.getDomain().allValues()) {
                offset = Math.min(i, offset);
                max = Math.max(i, max);
            }
        }
        union = BitVector.factory(max - offset + 1, false);
        queue = new LinkedList<Variable>();
        priority = getArity() * getArity();
    }

    @Override
    public boolean check() {
        final BitVector singletons = this.union;
        singletons.fill(false);
        final int offset = this.offset;

        final int[] tuple = this.tuple;
        for (int i = getArity(); --i >= 0;) {
            final int value = getVariable(i).getDomain().value(tuple[i]);
            if (singletons.get(value - offset)) {
                return false;
            }
            singletons.set(value - offset);
        }
        return true;
    }

    private boolean filter(Variable checkedVariable, int value,
            RevisionHandler revisator) {
        for (Variable v : getScope()) {
            if (v == checkedVariable) {
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

    @Override
    public boolean revise(final RevisionHandler revisator, final int reviseCount) {
        final int min = this.offset;
        final Deque<Variable> queue = this.queue;

        queue.clear();
        for (int pos = getArity(); --pos >= 0;) {
            if (getRemovals(pos) >= reviseCount
                    && getVariable(pos).getDomainSize() == 1) {
                queue.offer(getVariable(pos));
            }
        }

        while (!queue.isEmpty()) {
            final Variable checkedVariable = queue.poll();
            final int value = checkedVariable.getDomain().value(
                    checkedVariable.getFirst());

            if (filter(checkedVariable, value, revisator)) {
                return false;
            }
        }

        final BitVector union = this.union;
        union.fill(false);
        int size = 0;
        for (Variable v : getScope()) {
            for (int i = v.getFirst(); i >= 0; i = v.getNext(i)) {
                if (union.set(v.getDomain().value(i) - min)
                        && ++size >= getArity()) {
                    return true;

                }
                assert size == union.cardinality();
            }
        }

        return false;
    }

    public String toString() {
        return "allDifferent" + Arrays.toString(getScope());
    }

    @Override
    public float getEvaluation() {
        return priority;
    }
}
