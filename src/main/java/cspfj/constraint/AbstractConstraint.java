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

import java.util.HashSet;
import java.util.Set;
import java.util.logging.Logger;

import cspfj.constraint.extension.TupleManager;
import cspfj.filter.RevisionHandler;
import cspfj.problem.Variable;

public abstract class AbstractConstraint implements Constraint {
    private static final Logger LOGGER = Logger
            .getLogger(AbstractConstraint.class.getName());
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

    private final String name;

    protected int[] tuple;

    protected TupleManager tupleManager;

    private int weight = 1;

    private int entailedAtLevel = -1;

    public AbstractConstraint(final Variable... scope) {
        this(null, scope);
    }

    public AbstractConstraint(final String name, final Variable... scope) {
        this.scope = scope.clone();
        // scopeSet = new HashSet<Variable>(scope.length, 1);
        // for (Variable v : scope) {
        // assert v.getDomain() != null;
        // scopeSet.add(v);
        // }
        arity = scope.length;
        id = cId++;
        tuple = new int[arity];

        tupleManager = new TupleManager(this, tuple);

        if (name == null) {
            this.name = "C" + id;
        } else {
            this.name = name;
        }

    }

    @Override
    public final int getWeight() {
        return weight;
    }

    @Override
    public final void incWeight() {
        weight++;
    }

    public final int getValue(final int position) {
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
        if (scopeSet == null) {
            scopeSet = new HashSet<Variable>();
            for (Variable v : scope) {
                scopeSet.add(v);
            }
        }
        return scopeSet;
    }

    public final int getId() {
        return id;
    }

    public static final void resetCId() {
        cId = 0;
    }

    public final int getArity() {
        return arity;
    }

    @Override
    public final boolean equals(final Object object) {
        if (!(object instanceof Constraint)) {
            return false;
        }
        return id == ((Constraint) object).getId();
    }

    public final String getName() {
        return name;
    }

    public String getType() {
        return this.getClass().getSimpleName();
    }

    private int currentLevel;

    public final int getCurrentLevel() {
        return currentLevel;
    }

    @Override
    public void restore(final int level) {
        if (entailedAtLevel > level) {
            LOGGER.finest("Disentailing " + this);
            entailedAtLevel = -1;
        }
        currentLevel = level;
    }

    @Override
    public void setLevel(final int level) {
        currentLevel = level;
    }

    @Override
    public int hashCode() {
        return id;
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

    @Override
    public int getRemovals(final int position) {
        throw new UnsupportedOperationException();
    }

    @Override
    public void setRemovals(final int position, final int value) {
        // Ignore
    }

    @Override
    public void fillRemovals(final int value) {
        // Ignore
    }

    @Override
    public boolean hasNoRemovals(final int reviseCount) {
        return false;
    }

    @Override
    public final boolean isEntailed() {
        return entailedAtLevel >= 0;
    }

    @Override
    public final void entail() {
        entailedAtLevel = currentLevel;
        LOGGER.finer(currentLevel + ": entailing " + this);
    }

    private static final RevisionHandler NULL_REVISATOR = new RevisionHandler() {
        @Override
        public void revised(final Constraint constraint, final Variable variable) {
            // Nothing
        }
    };

    @Override
    public boolean isConsistent(final int reviseCount) {
        final int level = getCurrentLevel();
        for (Variable v : getScope()) {
            v.setLevel(level + 1);
        }
        setLevel(level + 1);

        final boolean consistent = revise(NULL_REVISATOR, reviseCount);

        for (Variable v : getScope()) {
            v.restoreLevel(level);
        }
        restore(level);

        return consistent;
    }

}
