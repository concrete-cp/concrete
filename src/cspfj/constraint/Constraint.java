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
import java.util.Collection;
import java.util.List;
// import java.util.logging.Logger;

import cspfj.exception.FailedGenerationException;
import cspfj.exception.MatrixTooBigException;
import cspfj.problem.Variable;

public abstract class Constraint {
    private final Variable[] involvedVariables;

    private final int[] positionInVariable;

    public final static int MAX_ARITY = 4;

    // private final Arc[] arcs;

    protected final int[][][] last;

    protected final boolean[][] lastCheck;

    private static int cId = 0;

    private final int id;

    protected final int[] tuple;

    private int weight = 1;

    private final int arity;

    private final boolean tupleCache;

    private static long checks = 0;

    private static long nbPresenceChecks = 0;

    private static int nbEffectiveRevisions = 0;

    private static int nbUselessRevisions = 0;

    // private static int nbSkippedRevisions = 0;

    // private static final Logger logger = Logger
    // .getLogger("cspfj.constraints.Constraint");

    private final int[] realTuple;

    private boolean active;

    protected final MatrixManager matrix;

    private final int[][] nbSupports;

    private final int[] nbMaxConflicts;

    private final int size;

    private final boolean removals[];

    private boolean changedConstraint = true;

    protected Constraint(final Variable[] scope) {
        involvedVariables = scope;
        arity = involvedVariables.length;

        tuple = new int[arity];
        realTuple = new int[arity];
        removals = new boolean[arity];
        id = cId++;
        tupleCache = useTupleCache();

        int maxDomain = 0;
        for (int i = arity; --i >= 0;) {
            if (involvedVariables[i].getDomain().length > maxDomain) {
                maxDomain = involvedVariables[i].getDomain().length;
            }
        }

        if (tupleCache) {
            last = new int[arity][maxDomain][arity];
            for (int i = arity; --i >= 0;) {
                for (int j = maxDomain; --j >= 0;) {
                    Arrays.fill(last[i][j], -1);
                }
            }
            lastCheck = new boolean[arity][maxDomain];
            // for (int i = arity; --i >= 0;) {
            // last[i] = new int[involvedVariables[i].getDomain().length][];
            // lastCheck[i] = new
            // boolean[involvedVariables[i].getDomain().length];
            // }

        } else {
            last = null;
            lastCheck = null;
        }
        // arcs = new Arc[arity];
        // for (int i = 0; i < arity; i++) {
        // arcs[i] = new Arc(this, i);
        // }
        active = false;
        // initLast();

        matrix = MatrixManager.factory(scope, tuple);

        try {
            matrix.init(true);
        } catch (MatrixTooBigException e) {
            matrix.clear();
        }

        nbMaxConflicts = new int[arity];
        nbSupports = new int[arity][];

        int size = 1;
        for (Variable v : involvedVariables) {
            size *= v.getDomain().length;
        }

        this.size = size;

        positionInVariable = new int[arity];
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

    public void initNbSupports() {
        if (!changedConstraint) {
            return;
        }
        for (int i = arity; --i >= 0;) {
            final int domainSize = involvedVariables[i].getDomain().length;
            nbSupports[i] = new int[domainSize];
            nbMaxConflicts[i] = 0;

            for (int j = domainSize; --j >= 0;) {
                nbSupports[i][j] = nbTuples(i, j);
                final int nbConflicts = (size / domainSize) - nbSupports[i][j];
                if (nbConflicts > nbMaxConflicts[i]) {
                    nbMaxConflicts[i] = nbConflicts;
                }
            }
        }
        changedConstraint = false;
    }

    public abstract boolean useTupleCache();

    public final boolean isInvolved(final Variable variable) {
        return getPosition(variable) >= 0;
    }

    public final int getPosition(final Variable variable) {
        for (int i = arity; --i >= 0;) {
            if (involvedVariables[i] == variable) {
                return i;
            }
        }
        return -1;
    }

    public final int getPositionInVariable(final Variable variable) {
        return positionInVariable[getPosition(variable)];
    }

    public final void setPositionInVariable(final Variable variable,
            final int position) {
        positionInVariable[getPosition(variable)] = position;
    }

    public final int[] getTuple() {
        return tuple;
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
        return matrix.isTrue(tuple);
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

    public final boolean skipRevision(final int position) {
        int size = 1;
        for (int i = arity; --i >= 0;) {
            if (i != position) {
                size *= involvedVariables[i].getDomainSize();
            }
        }

        // logger.info("size = " + size + ", maxConf = "
        // + nbMaxConflicts[position]);
        if (size > nbMaxConflicts[position]) {
            return true;
        }

        // return false;

        final boolean[] removals = this.removals;

        if (!removals[position]) {
            return false;
        }

        for (int i = arity; --i >= 0;) {
            if (i != position && removals[i]) {
                return false;
            }
        }
        //
        return true;
    }

    public final boolean revise(final Variable variable, final int level) {
        return revise(getPosition(variable), level);
    }

    public boolean revise(final int position, final int level) {
        final Variable variable = involvedVariables[position];

        assert !variable.isAssigned();

        // final boolean shouldBeSkipped;

        if (skipRevision(position)) {
            // shouldBeSkipped = true;
            // nbSkippedRevisions++;
            return false;
        }
        // else {
        // shouldBeSkipped = false;
        // }

        boolean revised = false;

        // logger.finer("Revising " + variable + " "
        // + Arrays.toString(variable.getCurrentDomain()) + " against "
        // + this);

        for (int index = variable.getFirst(); index >= 0; index = variable
                .getNext(index)) {

            // logger.finer("Checking (" + variable + ", " + index + ")");

            if (!findValidTuple(position, index)) {
                // if (shouldBeSkipped) {
                // logger.finer("removing " + index + " from " + variable);

                // final int toRemove = index;
                // index = variable.getPrev(index);

                variable.remove(index, level);

                revised = true;
                active = true;
                //
                // if (index < 0) {
                // index = variable.getFirst();
                //
                // if (index < 0) {
                // break;
                // }
                // }
            }

        }

        if (revised) {
            // assert !shouldBeSkipped : this + " " +
            // involvedVariables[position]
            // + " " + " " + nbMaxConflicts[position];
            nbEffectiveRevisions++;
        } else {
            nbUselessRevisions++;
        }

        return revised;
    }

    public void setFirstTuple(final int variablePosition, final int index) {
        for (int position = arity; --position >= 0;) {
            if (position == variablePosition) {
                tuple[position] = index;
            } else {
                tuple[position] = involvedVariables[position].getFirst();
            }
        }

    }

    public boolean setNextTuple(final int fixedVariablePosition) {
        final int[] tuple = this.tuple;
        final Variable[] involvedVariables = this.involvedVariables;
        for (int i = arity; --i >= 0;) {
            if (i == fixedVariablePosition) {
                continue;
            }

            final int index = involvedVariables[i].getNext(tuple[i]);

            if (index < 0) {
                tuple[i] = involvedVariables[i].getFirst();
            } else {
                tuple[i] = index;
                return true;
            }
        }
        return false;
    }

    protected boolean controlTuplePresence(final int position) {
        nbPresenceChecks++;
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

    protected boolean findValidTuple(final int variablePosition, final int index) {
        assert this.isInvolved(involvedVariables[variablePosition]);

        if (tupleCache && lastCheck[variablePosition][index]) {
            System.arraycopy(last[variablePosition][index], 0, tuple, 0, arity);

            if (controlTuplePresence(variablePosition)) {
                // System.out.print("c") ;
                return true;
            }
        }

        final MatrixManager matrix = this.matrix;

        if (!matrix.setFirstTuple(variablePosition, index)) {
            return false;
        }

        final boolean[][] lastCheck = this.lastCheck;
        final int arity = this.arity;
        final int[] tuple = this.tuple;
        final boolean tupleCache = this.tupleCache;
        final int[][][] last = this.last;

        // System.err.println(index+", "+ variablePosition+" : " +
        // Arrays.toString(tuple));

        do {
            if (check()) {
                if (tupleCache) {
                    for (int position = arity; --position >= 0;) {
                        final int value = tuple[position];

                        // if (myLast[value] == null) {
                        // myLast[value] = new int[arity];
                        // }
                        System.arraycopy(tuple, 0, last[position][value], 0,
                                arity);
                        lastCheck[position][value] = true;
                    }
                }
                return true;
            }
        } while (matrix.next());

        return false;

    }

    public int nbTuples(final int variablePosition, final int index) {
        // if (logger.isLoggable(Level.FINER)) {
        // logger.finer("Counting tuples : " + this + ", "
        // + involvedVariables[variablePosition] + ", " + index);
        // }

        if (arity > MAX_ARITY) {
            return 0;
        }

        if (!matrix.setFirstTuple(variablePosition, index)) {
            return 0;
        }

        int nbTuples = 0;

        do {
            if (check()) {
                nbTuples++;
            }
        } while (matrix.next());

        return nbTuples;
    }

    public final int getNbSupports(final Variable variable, final int index) {
        return getNbSupports(getPosition(variable), index);
    }

    public final int getNbSupports(final int position, final int index) {
        return nbSupports[position][index];
    }

    public final int getNbConflicts(final Variable variable, final int index) {
        return size - nbSupports[variable.getId()][index];
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

    public final double getFreedomDegree() {
        double count = -1;
        for (Variable v : involvedVariables) {
            if (v.getDomainSize() > 1) {
                count++;
            }
        }
        return Math.max(count / (arity - 1), 0);
    }

    public static final long getNbChecks() {
        return checks;
    }

    public final boolean checkFirst() {
        return checkFirstWith(involvedVariables[0], involvedVariables[0]
                .getFirst());

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
                // if (last[position][i] == null) {
                // last[position][i] = new int[arity];
                // }
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

    // public final boolean isInvolved(final Variable variable) {
    // for (Variable v : getInvolvedVariables()) {
    // if (v == variable) {
    // return true;
    // }
    // }
    // return false;
    // }

    public final int getArity() {
        return arity;
    }

    public boolean removeTuple(final List<Variable> scope,
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

        if (matrix.removeTuple(realTuple)) {
            if (useTupleCache()) {

                for (int p = arity; --p >= 0;) {
                    if (Arrays.equals(realTuple, last[p][realTuple[p]])) {
                        lastCheck[p][realTuple[p]] = false;
                    }
                }
            }
            changedConstraint = true;
            return true;
        }

        return false;

    }

    // public final Arc[] getArcs() {
    // return arcs;
    // }

    public final boolean isActive() {
        return active;
    }

    public final void setActive(final boolean active) {
        this.active = active;
    }

    public final static Constraint findConstraint(
            final Collection<Variable> scope,
            final Collection<Constraint> constraints) {
        final int arity = scope.size();
        for (Constraint c : constraints) {
            if (c.getArity() != arity) {
                continue;
            }
            boolean valid = true;
            for (Variable variable : c.getInvolvedVariables()) {
                if (!scope.contains(variable)) {
                    valid = false;
                    break;
                }
            }

            if (valid) {
                return c;
            }
        }
        return null;
    }

    public void intersect(final Variable[] scope, final boolean supports,
            final int[][] tuples) throws FailedGenerationException {
        try {
            matrix.intersect(scope, scope, supports, tuples);
        } catch (MatrixTooBigException e) {
            throw new FailedGenerationException(e.toString());
        }

    }

    public MatrixManager getMatrix() {
        return matrix;
    }

    public static final long getNbPresenceChecks() {
        return nbPresenceChecks;
    }

    public static final int getNbEffectiveRevisions() {
        return nbEffectiveRevisions;
    }

    // public static final int getNbSkippedRevisions() {
    // return nbSkippedRevisions;
    // }

    // public static final void incrementNbSkippedRevisions(int nbSR) {
    // nbSkippedRevisions += nbSR;
    // }

    public static final int getNbUselessRevisions() {
        return nbUselessRevisions;
    }

    public static final void clearStats() {
        checks = nbPresenceChecks = nbEffectiveRevisions = nbUselessRevisions = 0;
    }

    // public final int getRemovalCounter(final int position) {
    // return removalsCounter[position] ;
    // }

    public boolean getRemovals(final int variablePosition) {
        return removals[variablePosition];
    }

    //    
    public boolean getRemovals(final Variable variable) {
        return removals[getPosition(variable)];
    }

    //
    public void fillRemovals(final boolean removal) {
        Arrays.fill(removals, removal);

    }

    //
    public void setRemovals(final int variablePosition, final boolean removal) {
        removals[variablePosition] = removal;

    }

    public void setRemovals(final Variable variable, final boolean removal) {
        setRemovals(getPosition(variable), removal);

    }

    // public void setNbRemovals(Variable variable, int i) {
    // nbRecentRemovals[getPosition(variable)] = i;
    //
    // }

    // public void clearNoGoods() {
    // matrix.clear();
    // }

    // public final boolean haveMatrix() {
    // return matrix != null;
    // }

    // public void clearMatrix() {
    // matrix.clear();
    // }
    public boolean equals(Constraint c) {
        return id == c.getId();
    }
}
