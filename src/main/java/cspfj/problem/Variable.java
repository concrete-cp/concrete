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

import java.util.Collection;
import java.util.List;

import com.google.common.base.Predicates;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.Iterables;

import cspfj.constraint.Constraint;
import cspfj.constraint.DynamicConstraint;
import cspfj.priorityqueues.Identified;
import cspfj.util.BitVector;

public final class Variable implements Identified {

    private Constraint[] constraints;

    private List<DynamicConstraint> dynamicConstraints;

    private Domain domain;

    // private boolean assigned;

    private static int nbV = 0;

    private final int id;

    private int[] positionInConstraint;

    private final String name;

    public Variable(final String name, final Domain domain) {
        this.domain = domain;
        // assigned = false;
        id = nbV++;
        this.name = name;
    }

    /**
     * Reinit ID generator (before loading a new problem).
     */
    public static void resetVId() {
        nbV = 0;
    }

    @Override
    public String toString() {
        if (domain == null) {
            return name + " [?]";
        }
        return name + " " + domain;
    }

    /**
     * @param newConstraints
     *            Liste des contraintes impliquant la variable
     */
    public void setInvolvingConstraints(
            final Collection<Constraint> newConstraints) {
        this.constraints = newConstraints.toArray(new Constraint[newConstraints
                .size()]);
        final ImmutableList.Builder<DynamicConstraint> newDynamicConstraints = ImmutableList
                .builder();
        for (Constraint c : Iterables.filter(newConstraints,
                Predicates.instanceOf(DynamicConstraint.class))) {
            newDynamicConstraints.add((DynamicConstraint) c);
        }
        dynamicConstraints = newDynamicConstraints.build();

        positionInConstraint = new int[newConstraints.size()];
        for (int i = newConstraints.size(); --i >= 0;) {
            positionInConstraint[i] = constraints[i].getPosition(this);
        }

    }

    /**
     * @return La taille du domaine
     */
    public int getDomainSize() {
        return domain.size();
    }

    public void setSingle(final int index) {
        domain.setSingle(index);
    }

    public void remove(final int index) {
        // assert !assigned :
        // "Trying to remove a value from an assigned variable";
        assert domain.present(index);

        domain.remove(index);
    }

    public int getId() {
        return id;
    }

    public Constraint[] getInvolvingConstraints() {
        return constraints;
    }

    public List<DynamicConstraint> getDynamicConstraints() {
        return dynamicConstraints;
    }

    public Domain getDomain() {
        return domain;
    }

    public int getFirst() {
        return domain.firstIndex();
    }

    public int getLast() {
        return domain.lastIndex();
    }

    public void setDomain(final Domain domain) {
        this.domain = domain;
    }

    public int[] getCurrentIndexes() {
        if (domain == null) {
            return null;
        }
        return domain.currentIndexes();
    }

    public int[] getCurrentValues() {
        if (domain == null) {
            return null;
        }
        return domain.currentValues();
    }

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
        return domain.getBitVector();
    }

    public int getPositionInConstraint(final int constraint) {
        return positionInConstraint[constraint];
    }

    public void setLevel(final int level) {
        domain.setLevel(level);
    }

    public void restoreLevel(final int level) {
        domain.restoreLevel(level);
    }

    public void reset() {
        // if (isAssigned()) {
        // unassign();
        // }
        domain.restoreLevel(0);
    }

    public boolean isPresent(final int index) {
        return domain.present(index);
    }

    public int getValue(final int index) {
        return domain.value(index);
    }

    public int getFirstValue() {
        return domain.value(domain.firstIndex());
    }

    public void makeSingleton(final int value1) {
        domain.setSingle(value1);
    }

    public String getName() {
        return name;
    }
}
