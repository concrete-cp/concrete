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

import java.util.Set;
import java.util.TreeSet;
import java.util.logging.Logger;

import cspfj.problem.Variable;

public final class AllDifferentConstraint extends Constraint {

    private final static Set<Integer> union = new TreeSet<Integer>();;

    // private final Integer[] constants;

    private final static Logger logger = Logger
            .getLogger("cspfj.constraint.AllDifferentConstraint");

    // private boolean removedConstants = false;

    public AllDifferentConstraint(final Variable[] scope) {
        super(scope);
    }

    @Override
    public boolean check() {
        union.clear();
        // for (int i : constants) {
        // union.add(i);
        // }
        final Variable[] involvedVariables = getInvolvedVariables();
        final int[] tuple = this.tuple;
        for (int i = getArity(); --i >= 0;) {
            final int index = involvedVariables[i].getDomain()[tuple[i]];
            if (union.contains(index)) {
                return false;
            }
            union.add(index);
        }
        return true;
    }

    public boolean revise(final int position, final int level) {
        final Variable variable = getInvolvedVariables()[position];
        assert !variable.isAssigned();

        final Set<Integer> union = AllDifferentConstraint.union;

        union.clear();

        boolean revised = false;

        for (int checkPos = getArity(); --checkPos >= 0;) {
            final Variable checkedVariable = getInvolvedVariables()[checkPos];
            for (int i : checkedVariable) {

                union.add(checkedVariable.getDomain()[i]);

            }

            if (position != checkPos && checkedVariable.getDomainSize() == 1) {
                final int index = variable
                        .index(checkedVariable.getDomain()[checkedVariable
                                .getFirstPresentIndex()]);
                if (index >= 0 && variable.isPresent(index)) {
                    variable.remove(index, level);
                    revised = true;
                    setActive(true);
                }
            }
        }

        if (union.size() < getArity()) {
            variable.empty(level);
            setActive(true);
            return true;
        }

        logger.finest("done : " + revised);
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
}
