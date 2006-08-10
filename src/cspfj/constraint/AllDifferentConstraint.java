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
import java.util.Set;
import java.util.TreeSet;

import cspfj.problem.Variable;

public final class AllDifferentConstraint extends AbstractConstraint {

    private final static Set<Integer> union = new TreeSet<Integer>();;

    private final Integer[] constants;

    public AllDifferentConstraint(final Variable[] scope,
            final Integer[] constants) {
        super(scope);
        this.constants = constants;

    }

    public AllDifferentConstraint(final Variable[] scope) {
        this(scope, new Integer[0]);
    }

    @Override
    public boolean check() {
        union.clear();
        for (int i : constants) {
            union.add(i);
        }

        for (int i : tuple) {
            if (union.contains(i)) {
                return false;
            }
            union.add(i);
        }
        return true;
    }

    public boolean revise(final Variable variable, final int level) {
        assert !variable.isAssigned();

        union.clear();

        boolean revised = false;

        for (Variable checkedVariable : getInvolvedVariables()) {
            for (int i : checkedVariable) {

                union.add(checkedVariable.getDomain()[i]);

            }

            if (variable != checkedVariable
                    && checkedVariable.getDomainSize() == 1) {
                final int index = variable
                        .index(checkedVariable.getDomain()[checkedVariable
                                .getFirstPresentIndex()]);
                if (index >= 0 && variable.isPresent(index)) {
                    variable.remove(index, level);
                    revised = true;
                }
            }
        }

        for (int value : constants) {
            union.add(value);
            final int index = variable.index(value);
            if (variable.isPresent(index)) {
                variable.remove(index, level);
                revised = true;
            }
        }

        if (union.size() < this.getInvolvedVariables().length) {
            variable.empty(level);
            return true;
        }

        return revised;
    }

    public int getNbTuples(final Variable variable, final int index) {
        int nbTuples = 1;
        for (Variable v : this.getInvolvedVariables()) {
            if (v == variable) {
                continue;

            }
            nbTuples *= v.getDomainSize();
        }
        return nbTuples;
    }

    public boolean useTupleCache() {
        return false;
    }

    @Override
    public String toString() {
        return super.toString() + " " + Arrays.toString(constants);
    }

}
