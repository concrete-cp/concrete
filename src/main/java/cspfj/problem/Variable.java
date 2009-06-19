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

package cspfj.problem;

import java.util.ArrayList;
import java.util.List;

import cspfj.constraint.Constraint;
import cspfj.constraint.DynamicConstraint;
import cspfj.util.BitVector;
import cspfj.util.Identified;

public final class Variable implements Cloneable, Identified {

    private Constraint[] constraints;

    private DynamicConstraint[] dynamicConstraints;

    // private int[] removed;
    private Domain domain;

    // private int assignedIndex;
    private boolean assigned;

    private static int nbV = 0;

    private final int id;

    private final String name;

    private int[] positionInConstraint;

    // private int[] weights;

    // private static final Logger logger = Logger.getLogger("cspfj.Variable");

    public Variable(final int[] dom) {
        this(dom, null);
    }

    /**
     * @param dom
     *            Array representings the domain elements
     * @param name
     *            Name of the variable
     */
    public Variable(final int[] dom, String name) {
        this.domain = new BitVectorDomain(dom);

        // this.removed = new int[domain.length];
        // Arrays.fill(removed, -1);

        // this.assignedIndex = -1;
        assigned = false;
        id = nbV++;
        // savedLevels = BitVector.newBitVector(1, false);
        if (name == null) {
            this.name = "X" + id;
        } else {
            this.name = name;
        }
    }

    // public void attachWeights(int[] weights) {
    // this.weights = weights;
    // }

    /**
     * Reinit ID generator (before loading a new problem).
     */
    public static void resetVId() {
        nbV = 0;
    }

    @Override
    public String toString() {
        return name + "[" + getDomainSize() + "]";
    }

    /**
     * @param constraints2
     *            Liste des contraintes impliquant la variable
     */
    public void setInvolvingConstraints(final Constraint[] constraints2) {
        this.constraints = constraints2;
        final List<DynamicConstraint> dynamicConstraints = new ArrayList<DynamicConstraint>();
        for (Constraint c : constraints2) {
            if (c instanceof DynamicConstraint) {
                dynamicConstraints.add((DynamicConstraint) c);
            }
        }

        this.dynamicConstraints = dynamicConstraints
                .toArray(new DynamicConstraint[dynamicConstraints.size()]);

        positionInConstraint = new int[constraints2.length];
        for (int i = constraints2.length; --i >= 0;) {
            updatePositionInConstraint(i);
        }

    }

    public void updatePositionInConstraint(final int constraintPosition) {
        positionInConstraint[constraintPosition] = constraints[constraintPosition]
                .getPosition(this);
        constraints[constraintPosition].setPositionInVariable(
                positionInConstraint[constraintPosition], constraintPosition);
    }

    /**
     * @return La taille du domaine
     */
    public int getDomainSize() {
        return domain.size();
    }

    public boolean isAssigned() {
        return assigned;
    }

    // public boolean assignNotAC(final int index, final Problem problem) {
    // assgn(index, problem);
    // return checkIndexValidity(index);
    // }

    // public void firstAssign(final int index) {
    // assignedIndex = index;
    // assigned = true;
    // }
    //
    // public void resetAssign() {
    // assigned = false;
    // }

    // private boolean checkIndexValidity(final int index) {
    // for (int c = constraints.length; --c >= 0;) {
    // if (!constraints[c].findValidTuple(positionInConstraint[c], index)) {
    // return false;
    // }
    // }
    // return true;
    // }

    public void assign(final int index, final Problem problem) {
        assert !assigned;
        assert domain.present(index);

        // assignedIndex = index;
        assigned = true;

        domain.setSingle(index);

        // assert !reinitBooleanDomain();
        problem.decreaseFutureVariables();
    }

    public void unassign(final Problem problem) {
        assert assigned;
        assigned = false;
        problem.increaseFutureVariables();
        // System.arraycopy(beforeAssignDomain, 0, bitDomain, 0,
        // bitDomain.length);
    }

    public void remove(final int index) {
        assert !assigned : "Trying to remove a value from an assigned variable";
        assert domain.present(index);

        domain.remove(index);
    }

    // private void pushLevel(int level) {
    // int saveTo = level-1;
    // if (saveTo >= history.length) {
    // // Expand tables
    // history = Arrays.copyOf(history, saveTo + 1);
    // savedLevels = Arrays.copyOf(savedLevels, BitVector
    // .nbWords(saveTo + 1));
    //
    // history[saveTo] = bitDomain.clone();
    // } else if (history[saveTo] == null) {
    // history[saveTo] = bitDomain.clone();
    // } else {
    // System.arraycopy(bitDomain, 0, history[saveTo], 0,
    // bitDomain.length);
    // }
    //
    // BitVector.set(savedLevels, saveTo);
    // }

    // public void restore(final int level) {
    // assert !assigned;
    //
    // //assert level > 0;
    // final int[] removed = this.removed;
    //
    // for (int i = getLastAbsent(); i >= 0; i = getPrevAbsent(i)) {
    // if (removed[i] >= level) {
    // assert !isPresent(i);
    // domainSize++;
    // removed[i] = -1;
    // BitVector.set(bitDomain, i, true);
    // }
    // }
    //
    // // final long[] restored = popLevel(level);
    // //
    // // assert BitVector.equals(bitDomain, restored) : this + " : "
    // // + BitVector.toString(bitDomain) + " / "
    // // + BitVector.toString(restored) + " / " + historyTS();
    //
    // }

    // private String historyTS() {
    // final StringBuilder stb = new StringBuilder();
    // for (int i = BitVector.nextSetBit(savedLevels, 0); i >= 0; i = BitVector
    // .nextSetBit(savedLevels, i + 1)) {
    // stb.append(i).append(": ").append(BitVector.toString(history[i])).append(" - ");
    // }
    // return stb.toString();
    // }
    //
    // private long[] popLevel(int level) {
    // final int levelToRestore = BitVector.prevSetBit(savedLevels, level + 1);
    // BitVector.clearFrom(savedLevels, level);
    // return history[levelToRestore];
    //
    // }

    public int getId() {
        return id;
    }

    // public long getNbSupports(final int index) {
    // long nbSupports = 0;
    // for (Constraint c : constraints) {
    // final long nbSup = c.getNbSupports(this, index);
    // if (nbSup <= 0) {
    // return -1;
    // }
    // nbSupports += nbSup;
    // }
    // return nbSupports;
    // }

    public Constraint[] getInvolvingConstraints() {
        return constraints;
    }

    public DynamicConstraint[] getDynamicConstraints() {
        return dynamicConstraints;
    }

    public Domain getDomain() {
        return domain;
    }

    public int getFirst() {
        return domain.first();
    }

    public int getLast() {
        return domain.last();
    }

    public int[] getCurrentIndexes() {
        final int[] indexes = new int[domain.size()];

        for (int i = getFirst(), j = 0; i >= 0; i = getNext(i), j++) {
            indexes[j] = i;
        }

        return indexes;
    }

    public String getName() {
        return name;
    }

    // public int getRemovedLevel(final int index) {
    // return removed[index];
    // }

    public int getNext(final int index) {
        return domain.next(index);
    }

    public int getPrev(final int index) {
        return domain.prev(index);
    }

    public int getLastAbsent() {
        return domain.lastAbsent();
    }

    public int getPrevAbsent(final int index) {
        return domain.prevAbsent(index);
    }

    public BitVector getBitDomain() {
        return ((BitVectorDomain) domain).getBitVector();
    }

    // public boolean reinitBooleanDomain() {
    // boolean change = false;
    // if (assigned) {
    // for (int i = removed.length; --i >= 0;) {
    // change |= BitVector.set(bitDomain, i, i == assignedIndex);
    // }
    // } else {
    // for (int i = removed.length; --i >= 0;) {
    // change |= BitVector.set(bitDomain, i, removed[i] < 0);
    // }
    // }
    // return change;
    // }

    public Variable clone() throws CloneNotSupportedException {
        final Variable variable = (Variable) super.clone();

        variable.domain = domain.clone();

        // variable.beforeAssignDomain = bitDomain.clone();

        return variable;
    }

    // public void reAssign(final int index) {
    // assignedIndex = index;
    // }

    public int getPositionInConstraint(final int constraint) {
        return positionInConstraint[constraint];
    }

    public void setLevel(int level) {
        domain.setLevel(level);
    }

    public void restoreLevel(int level) {
        domain.restoreLevel(level);
    }

    public void reset(Problem problem) {
        if (isAssigned()) {
            unassign(problem);
        }
        domain.restoreLevel(0);
    }

    public boolean isPresent(int index) {
        return domain.present(index);
    }

    public int getValue(int index) {
        return domain.value(index);
    }

    public void makeSingleton(int value1) {
        domain.setSingle(value1);
    }
}
