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

package cspfj.constraint;

import java.util.Arrays;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import cspfj.problem.Variable;

public abstract class Constraint {
    private final Variable[] involvedVariables;

    private final Arc[] arcs;

    private final int[][][] last;

    private final boolean[][] lastCheck;

    private static int cId = 0;

    private final int id;

    protected final int[] tuple;

    private int weight = 1;

    private final int arity;

    private final boolean tupleCache;

    private static long checks = 0;

    private static final Logger logger = Logger
            .getLogger("cspjf.constraints.AbstractConstraint");

    private boolean[] matrix = null;

    private final int[] realTuple;

    private boolean active;

    // private final boolean[] recentlyRemoved;

    protected Constraint(final Variable[] scope) {
        involvedVariables = scope;
        arity = involvedVariables.length;

        tuple = new int[arity];
        realTuple = new int[arity];
        // recentlyRemoved = new boolean[arity];
        id = cId++;
        tupleCache = useTupleCache();
        if (tupleCache) {
            last = new int[arity][][];
            lastCheck = new boolean[arity][];
            for (int i = 0; i < arity; i++) {
                last[i] = new int[involvedVariables[i].getDomain().length][];
                lastCheck[i] = new boolean[involvedVariables[i].getDomain().length];
            }

        } else {
            last = null;
            lastCheck = null;
        }
        arcs = new Arc[arity];
        for (int i = 0; i < arity; i++) {
            arcs[i] = new Arc(this, i);
        }
        active = false;
        // initLast();
    }

    // public final void initLast() {
    //
    // for (int[] l : last) {
    // for (int i = 0; i < l.length; i++) {
    // l[i] = 0;
    // }
    // }
    //
    // }

    public abstract boolean useTupleCache();

    public final boolean isInvolving(final Variable variable) {
        return getPosition(variable) >= 0;
    }

    public final int getPosition(final Variable variable) {
        int rValue = -1;
        for (int i = 0; i < arity; i++) {
            if (involvedVariables[i] == variable) {
                rValue = i;
                break;
            }
        }
        return rValue;
    }

    public final Variable[] getInvolvedVariables() {
        return involvedVariables;
    }

    // public String toString() {
    // StringBuffer sb = new StringBuffer();
    // for (boolean[] line : matrix) {
    // sb.append(Arrays.toString(line));
    // sb.append('\n');
    // }
    // return sb.toString();
    // }

    public final int getId() {
        return id;
    }

    public final static void resetCId() {
        cId = 0;
    }

    // public int getWeight() {
    // return weight;
    // }
    //
    // public void increaseWeight() {
    // //System.out.println("Increasing "+this);
    // this.weight++;
    // }

    public String toString() {
        return "C" + id + " " + Arrays.toString(involvedVariables);
    }

    public boolean check() {
        checks++;
        return matrix == null || matrix[matrixIndex(tuple)];
    }

    // public boolean revise(final int level) {
    // boolean revised = false;
    // for (Variable v : involvedVariables) {
    // // System.out.println("revising " + v);
    // assert v.getDomainSize() > 0 : v + " is empty !";
    //
    // if (v.getDomainSize() > 1 && revise(v, level)) {
    // if (v.getDomainSize() < 1) {
    // increaseWeight();
    // return true;
    // }
    // revised = true;
    // }
    // }
    //
    // return revised;
    // }

    public final boolean revise(final Variable variable, final int level) {
        return revise(getPosition(variable), level);
    }

    public boolean revise(final int position, final int level) {
        final Variable variable = involvedVariables[position];

        assert !variable.isAssigned();

        boolean revised = false;

        for (int index : variable) {

            // logger.finest("Checking (" + variable + ", " + index+")") ;

            if (!findValidTuple(position, index)) {

                // logger.finest("removing " + index + " from " + variable);
                variable.remove(index, level);
                revised = true;
                active = true;
            }

        }

        return revised;
    }

    private void setFirstTuple(final int variablePosition, final int index) {
        for (int position = 0; position < arity; position++) {
            if (position == variablePosition) {
                tuple[position] = index;
            } else {
                tuple[position] = involvedVariables[position]
                        .getFirstPresentIndex();
            }
        }

    }

    private boolean setNextTuple(final int fixedVariablePosition) {
        final int[] tuple = this.tuple;
        final Variable[] involvedVariables = this.involvedVariables;
        for (int i = arity; --i >= 0;) {
            if (i == fixedVariablePosition) {
                continue;
            }

            final int index = involvedVariables[i].getNext(tuple[i]);

            if (index < 0) {
                tuple[i] = involvedVariables[i].getFirstPresentIndex();
            } else {
                tuple[i] = index;
                return true;
            }
        }
        return false;
    }

    private boolean controlTuplePresence(final int position) {
        final int[] tuple = this.tuple;
        final Variable[] involvedVariables = this.involvedVariables;
        for (int i = arity; --i >= 0;) {
            if (i != position && !involvedVariables[i].isPresent(tuple[i])) {
                return false;
            }
        }

        return true;

    }

    public final boolean findValidTuple(final Variable variable, final int index) {
        return findValidTuple(getPosition(variable), index);
    }

    private boolean findValidTuple(final int variablePosition, final int index) {
        assert this.isInvolving(involvedVariables[variablePosition]);

        final int[] lastTuple;

        if (tupleCache) {
            lastTuple = last[variablePosition][index];
        } else {
            lastTuple = null;
        }

        if (lastTuple != null && lastCheck[variablePosition][index]) {
            System.arraycopy(lastTuple, 0, tuple, 0, arity);

            if (controlTuplePresence(variablePosition)) {
                // System.out.print("c") ;
                return true;
            }

        }

        setFirstTuple(variablePosition, index);

        do {
            if (check()) {
                if (tupleCache) {
                    for (int position = arity; --position >= 0;) {
                        final int value = tuple[position];
                        if (last[position][value] == null) {
                            last[position][value] = new int[arity];
                        }
                        System.arraycopy(tuple, 0, last[position][value], 0,
                                arity);
                        lastCheck[position][value] = true;
                    }
                }
                return true;
            }
        } while (setNextTuple(variablePosition));

        return false;

    }

    public int getNbTuples(final Variable variable, final int index) {
        assert this.isInvolving(variable);

        if (logger.isLoggable(Level.FINER)) {
            logger.finer("Counting tuples : " + this + ", " + variable + ", "
                    + index);
        }
        final int variablePosition = getPosition(variable);

        int nbTuples = 0;
        setFirstTuple(variablePosition, index);

        do {
            if (check()) {
                nbTuples++;
            }
        } while (setNextTuple(variablePosition));

        return nbTuples;
    }

    public final void increaseWeight() {
        weight++;
    }

    public final int getWeight() {
        return weight;
    }

    public final void setWeight(final int weight) {
        this.weight = weight;
    }

    public final float getFreedomDegree() {
        int count = -1;
        for (Variable v : involvedVariables) {
            if (v.getDomainSize() > 1) {
                count++;
            }
        }
        return Math.max((float) count / (arity - 1), 0);
    }

    public static final void resetChecks() {
        checks = 0;
    }

    public static final long getNbChecks() {
        return checks;
    }

    public final boolean checkFirst() {
        return checkFirstWith(involvedVariables[0], involvedVariables[0]
                .getFirstPresentIndex());

    }

    public final boolean checkFirstWith(final Variable variable, final int index) {

        final int variablePosition = getPosition(variable);
        setFirstTuple(variablePosition, index);
        if (tupleCache && Arrays.equals(tuple, last[variablePosition][index])) {
            assert check() == lastCheck[variablePosition][index] : Arrays
                    .toString(tuple)
                    + " = "
                    + Arrays.toString(last[variablePosition][index])
                    + ", "
                    + check()
                    + " /= "
                    + lastCheck[variablePosition][index];
            return lastCheck[variablePosition][index];
        }

        final boolean result = check();

        if (tupleCache) {
            for (int position = arity; --position >= 0;) {
                final int i = tuple[position];
                if (last[position][i] == null) {
                    last[position][i] = new int[arity];
                }
                System.arraycopy(tuple, 0, last[position][i], 0, arity);
                lastCheck[position][i] = result;
            }

        }

        return result;
    }

    //
    // public boolean checkFirstWith(final Variable variable, final int index) {
    // for (int i = 0; i < involvedVariables.length; i++) {
    // final Variable involvedVariable = involvedVariables[i];
    // if (variable.equals(involvedVariable)) {
    // tuple[i] = index;
    // } else {
    // tuple[i] = involvedVariable.getFirstPresentIndex();
    // }
    // }
    // return check();
    // }

    public final boolean isInvolved(final Variable variable) {
        for (Variable v : getInvolvedVariables()) {
            if (v == variable) {
                return true;
            }
        }
        return false;
    }

    public final int getArity() {
        return arity;
    }

    protected final void initMatrix(final boolean initialState) {
        int nbValues = 1;
        for (Variable v : involvedVariables) {
            nbValues *= v.getDomain().length;
        }

        final boolean[] matrix = this.matrix = new boolean[nbValues];

        Arrays.fill(matrix, initialState);
    }

    private final int matrixIndex(final int[] tuple) {
        final Variable[] involvedVariables = this.involvedVariables;
        int index = 0;
        for (int i = arity; --i >= 0;) {
            int skip = 1;
            for (int j = i; --j >= 0;) {
                skip *= involvedVariables[j].getDomain().length;
            }
            index += skip * tuple[i];
        }
        return index;
    }

    public final boolean removeTuple(final List<Variable> scope,
            final List<Integer> tuple) {

        final int[] realTuple = this.realTuple;
        final Variable[] involvedVariables = this.involvedVariables;

        for (int i = arity; --i >= 0;) {
            for (int j = arity; --j >= 0;) {
                if (scope.get(i) == involvedVariables[j]) {
                    realTuple[j] = tuple.get(i);
                    break;
                }
            }
        }

        return removeTuple(realTuple);

    }

    public final boolean removeTuple(final int[] tuple) {

        if (this.matrix == null) {
            initMatrix(true);
        }
        final boolean[] matrix = this.matrix;
        final int matrixIndex = matrixIndex(tuple);

        if (matrix[matrixIndex]) {
            matrix[matrixIndex] = false;

            if (useTupleCache()) {
                for (int p = arity; --p >= 0;) {
                    if (Arrays.equals(tuple, last[p][tuple[p]])) {
                        lastCheck[p][tuple[p]] = false;

                    }
                }
            }
            return true;
        }
        return false;
    }

    public final void setMatrixIndex(final int[] indexes, final boolean value) {
        matrix[matrixIndex(indexes)] = value;
    }

    public final Arc[] getArcs() {
        return arcs;
    }

    public void freeLast(final int position, final int index) {
        if (!tupleCache) {
            return;
        }
        last[position][index] = null;
    }

    public void freeLast(final Variable variable, final int index) {
        if (!tupleCache) {
            return;
        }
        freeLast(getPosition(variable), index);
    }

    public final boolean isActive() {
        return active;
    }

    public final void setActive(final boolean active) {
        this.active = active;
    }

    public void freeAll() {
        for (int v = arity; --v >= 0;) {
            for (int i = involvedVariables[v].getDomain().length; --i >= 0;) {
                freeLast(v, i);
            }
        }

    }

}
