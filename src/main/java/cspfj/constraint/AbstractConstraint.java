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
import java.util.HashSet;
import java.util.Set;

import cspfj.constraint.extension.TupleManager;
import cspfj.problem.Variable;

public abstract class AbstractConstraint implements Cloneable, Constraint {
    private static int cId = 0;

    private static long nbPresenceChecks = 0;

    public static final long getPresenceChecks() {
        return nbPresenceChecks;
    }

    public static void clearStats() {
        nbPresenceChecks = 0;
    }

    private Variable[] scope;

    private Set<Variable> scopeSet;

    private final int id;

    private final int arity;

    private boolean active;

    private final String name;

    protected int[] tuple;

    protected TupleManager tupleManager;

    private int weight = 1;

    private final int[] removals;

    public AbstractConstraint(final Variable... scope) {
        this(null, scope);
    }

    public AbstractConstraint(final String name, final Variable... scope) {
        this.scope = scope.clone();
        scopeSet = new HashSet<Variable>(scope.length, 1);
        for (Variable v : scope) {
            assert v.getDomain() != null;
            scopeSet.add(v);
        }
        arity = scope.length;
        id = cId++;
        tuple = new int[arity];

        tupleManager = new TupleManager(this, tuple);

        active = false;

        if (name == null) {
            this.name = "C" + id;
        } else {
            this.name = name;
        }

        removals = new int[arity];// new boolean[arity];
    }

    public int getRemovals(int position) {
        return removals[position];
    }

    public void setRemovals(int position, int value) {
        removals[position] = value;
    }

    public void fillRemovals(final int value) {
        Arrays.fill(removals, value);
    }

    public boolean hasNoRemovals(final int value) {
        for (int i : removals) {
            if (i >= value) {
                return false;
            }
        }
        return true;
    }

    @Override
    public int getWeight() {
        return weight;
    }

    @Override
    public void incWeight() {
        weight++;
    }

    @Override
    public void setWeight(final int weight) {
        this.weight = weight;
    }

    public int getValue(final int position) {
        return scope[position].getValue(tuple[position]);
    }

    public final boolean isInvolved(final Variable variable) {
        return getPosition(variable) >= 0;
    }

    public final int getPosition(final Variable variable) {
        for (int i = arity; --i >= 0;) {
            if (scope[i] == variable) {
                return i;
            }
        }

        return -1;
    }

    public final int[] getTuple() {
        return tuple;
    }

    public final Variable getVariable(final int position) {
        return scope[position];
    }

    public final Variable[] getScope() {
        return scope;
    }

    public final Set<Variable> getScopeSet() {
        return scopeSet;
    }

    public final int getId() {
        return id;
    }

    public final static void resetCId() {
        cId = 0;
    }

    public final boolean isBound(Variable variable) {
        for (Variable v : scope) {
            if (v != variable && v.getDomainSize() > 1) {
                return true;
            }
        }
        return false;
    }

    //
    // public final boolean check(int[] tuple) {
    // System.arraycopy(tuple, 0, this.tuple, 0, arity);
    // return chk();
    // }
    //
    // public final boolean checkFirstWith(final int variablePosition,
    // final int index) {
    // tupleManager.setFirstTuple(variablePosition, index);
    // return chk();
    // }

    public final int getArity() {
        return arity;
    }

    public final boolean isActive() {
        return active;
    }

    public final void setActive(final boolean active) {
        this.active = active;
    }

    public boolean equals(final Object object) {
        if (!(object instanceof Constraint)) {
            return false;
        }
        return id == ((Constraint) object).getId();
    }

    public AbstractConstraint deepCopy(final Collection<Variable> variables)
            throws CloneNotSupportedException {
        final AbstractConstraint constraint = this.clone();

        constraint.scope = new Variable[arity];

        for (int i = arity; --i >= 0;) {
            for (Variable v : variables) {
                if (v == scope[i]) {
                    constraint.scope[i] = v;
                    break;
                }
            }
        }

        return constraint;
    }

    public AbstractConstraint clone() throws CloneNotSupportedException {
        final AbstractConstraint constraint = (AbstractConstraint) super
                .clone();

        constraint.tuple = new int[arity];

        // constraint.positionInVariable fixe
        // constraint.nbMaxConflicts fixe
        // constraint.nbSupports fixe

        constraint.tupleManager = new TupleManager(constraint, constraint.tuple);
        return constraint;
    }

    public String getName() {
        return name;
    }

    public String getType() {
        return this.getClass().getSimpleName();
    }

    public void restore(final int level) {
        // Nothing here
    }

    public void setLevel(final int level) {
        // Nothing here
    }

    public int hashCode() {
        return id;
    }

    public int getEvaluation(int reviseCount) {
        return Integer.MAX_VALUE;
    }

    public final boolean controlTuplePresence(final int[] tuple) {
        nbPresenceChecks++;
        final Variable[] involvedVariables = getScope();
        for (int i = getArity(); --i >= 0;) {
            if (!involvedVariables[i].isPresent(tuple[i])) {
                return false;
            }
        }

        return true;
    }
}
