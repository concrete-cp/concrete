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

package cspfj.constraint.extension;

import java.util.Collection;
import java.util.Iterator;

import cspfj.constraint.AbstractConstraint;
import cspfj.constraint.extension.MatrixManagerDynamic.LLIterator;
import cspfj.exception.FailedGenerationException;
import cspfj.filter.RevisionHandler;
import cspfj.problem.Variable;
import cspfj.util.BitVector;

public class ExtensionConstraintDynamic extends AbstractConstraint implements
        ExtensionConstraint {

    private final MatrixManagerDynamic dynamic;

    private final BitVector[] toFind;

//    private final static Logger logger = Logger
//            .getLogger(ExtensionConstraintDynamic.class.getSimpleName());

    // public static boolean quick = false;

    public ExtensionConstraintDynamic(final Variable[] scope,
            final TupleSet matrix, final boolean shared)
            throws FailedGenerationException {
        super(scope);
        this.dynamic = new MatrixManagerDynamic(scope, matrix, shared);
        toFind = initFound();
    }

    public ExtensionConstraintDynamic(final Variable[] scope,
            final TupleSet matrix, final String name, final boolean shared)
            throws FailedGenerationException {
        super(name, scope);
        this.dynamic = new MatrixManagerDynamic(scope, matrix, shared);
        toFind = initFound();
    }

    private BitVector[] initFound() {

        final BitVector[] found = new BitVector[getArity()];
        for (int i = getArity(); --i >= 0;) {
            found[i] = BitVector.factory(getVariable(i).getDomain().maxSize(),
                    false);
        }
        return found;
    }

    public void setLevel(final int level) {
        dynamic.setLevel(level);
    }

    public void restore(final int level) {
        dynamic.restore(level);
        if (level <= 0) {
            for (final LLIterator itr = dynamic.iterator(); itr.hasNext();) {
                final int[] tuple = itr.next();
                if (!dynamic.isTrue(tuple)) {
                    itr.remove(-1);
                }
            }
        }

    }

    public boolean revise(final RevisionHandler revisator) {
        return reviseSTR(revisator);
    }

    private boolean reviseRM(final RevisionHandler revisator) {
        // logger.fine("Revising "+this);
        int nbToFind = 0;

        for (int i = getArity(); --i >= 0;) {

            final Variable var = getVariable(i);
            if (var.isAssigned()) {
                continue;
            }
            final BitVector toFind = this.toFind[i];
            toFind.fill(false);
            for (int index = var.getFirst(); index >= 0; index = var
                    .getNext(index)) {
                toFind.set(index);
                nbToFind++;
            }
        }

        for (final LLIterator itr = dynamic.iterator(); itr.hasNext();) {
            final int[] tuple = itr.next();
            assert dynamic.isTrue(tuple);
            if (controlTuplePresence(tuple)) {
                final int toFindBefore = nbToFind;
                for (int i = getArity(); --i >= 0;) {
                    if (toFind[i].get(tuple[i])) {
                        toFind[i].clear(tuple[i]);
                        if (--nbToFind <= 0) {
                            break;
                        }
                    }
                }
                if (nbToFind < toFindBefore) {
                    itr.promote();
                }
            }
        }

        return filter(toFind, revisator);
    }

    private boolean reviseSTR(final RevisionHandler revisator) {
        // logger.fine("Revising "+this);
        for (int i = getArity(); --i >= 0;) {
            final Variable var = getVariable(i);
            if (var.isAssigned()) {
                continue;
            }
            final BitVector toFind = this.toFind[i];
            toFind.fill(false);
            for (int index = getVariable(i).getFirst(); index >= 0; index = getVariable(
                    i).getNext(index)) {
                toFind.set(index);
            }

        }

        for (final LLIterator itr = dynamic.iterator(); itr.hasNext();) {
            final int[] tuple = itr.next();
            assert dynamic.isTrue(tuple);
            if (controlTuplePresence(tuple)) {
                for (int i = getArity(); --i >= 0;) {
                    toFind[i].clear(tuple[i]);
                }

            } else {
                itr.remove();
            }
        }

        return filter(toFind, revisator);
    }

    private boolean filter(BitVector[] toFinds, RevisionHandler revisator) {
        for (int i = getArity(); --i >= 0;) {
            final Variable variable = getVariable(i);
            if (variable.isAssigned()) {
                continue;
            }
            boolean rev = false;
            final BitVector toFind = toFinds[i];
            for (int index = toFind.nextSetBit(0); index >= 0; index = toFind
                    .nextSetBit(index + 1)) {
                variable.remove(index);
                rev = true;
            }

            if (rev) {
                if (variable.getDomainSize() <= 0) {
                    return false;
                }
                revisator.revised(this, variable);
                setActive(true);
            }
        }
        return true;
    }

    private boolean controlTuplePresence(final int[] tuple) {
        for (int i = tuple.length; --i >= 0;) {
            if (!getVariable(i).isPresent(tuple[i])) {
                return false;
            }
        }
        return true;
    }

    public ExtensionConstraintDynamic deepCopy(
            final Collection<Variable> variables)
            throws CloneNotSupportedException {
        final ExtensionConstraintDynamic constraint = (ExtensionConstraintDynamic) super
                .deepCopy(variables);
        // constraint.matrix = matrix.deepCopy(constraint.getScope(),
        // constraint.tuple);
        return constraint;
    }

    @Override
    public boolean check() {
        return dynamic.check();
    }

    @Override
    public boolean removeTuple(int[] tuple) {
        if (dynamic.removeTuple(tuple)) {
//            addConflict(tuple);
            return true;
        }
        return false;
    }

    private static boolean match(int[] tuple, int[] base) {
        assert tuple.length == base.length;
        for (int i = tuple.length; --i >= 0;) {
            if (base[i] >= 0 && base[i] != tuple[i]) {
                return false;
            }
        }
        return true;
    }

    @Override
    public int removeTuples(int[] base) {
        dynamic.unshareMatrix();
        int removed = 0;
        for (final Iterator<int[]> itr = dynamic.hsIterator(); itr.hasNext();) {
            final int[] currentTuple = itr.next();
            if (match(currentTuple, base)) {
                // logger.fine("Removing " + Arrays.toString(currentTuple));
                itr.remove();
                assert !dynamic.isTrue(currentTuple);
                removed++;
            }
        }
        return removed;
    }

    @Override
    public MatrixManager getMatrixManager() {
        return dynamic;
    }

    // @Override
    // public int removeTuples(int[] base) {
    // int removed = 0;
    // tupleManager.setFirstTuple(base);
    // do {
    // if (removeTuple(this.tuple)) {
    // removed++;
    // }
    // } while (tupleManager.setNextTuple(base));
    // return removed;
    // }

}
