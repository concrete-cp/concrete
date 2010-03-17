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

import java.util.LinkedList;

import cspfj.constraint.AbstractConstraint;
import cspfj.exception.FailedGenerationException;
import cspfj.filter.RevisionHandler;
import cspfj.generator.ConstraintManager;
import cspfj.problem.Domain;
import cspfj.problem.Problem;
import cspfj.problem.Variable;
import cspom.constraint.CSPOMConstraint;
import cspom.variable.CSPOMVariable;

public final class Gt extends AbstractConstraint {

    final private boolean strict;

    final private boolean ordered;

    static {
        ConstraintManager.register("gt", Gt.class);
        ConstraintManager.register("lt", Gt.class);
        ConstraintManager.register("ge", Gt.class);
        ConstraintManager.register("le", Gt.class);
    }

    public Gt(final Variable v0, final Variable v1, final boolean strict) {
        super(v0, v1);
        this.strict = strict;
        this.ordered = v0.getDomain().isOrdered() && v1.getDomain().isOrdered();
    }

    @Override
    public boolean check() {
        if (strict) {
            return getValue(0) > getValue(1);
        }
        return getValue(0) >= getValue(1);
    }

    private int min(final int position) {
        final Domain dom = getVariable(position).getDomain();
        if (ordered) {
            return dom.value(dom.first());
        }
        int min = Integer.MAX_VALUE;
        for (int i = dom.first(); i >= 0; i = dom.next(i)) {
            if (dom.value(i) < min) {
                min = dom.value(i);
            }
        }
        return min;
    }

    private int max(final int position) {
        final Domain dom = getVariable(position).getDomain();
        if (ordered) {
            return dom.value(dom.last());
        }
        int max = Integer.MIN_VALUE;
        for (int i = dom.first(); i >= 0; i = dom.next(i)) {
            if (dom.value(i) > max) {
                max = dom.value(i);
            }
        }
        return max;
    }

    private boolean removeGt(final int value, final int position) {
        boolean removed = false;
        final Domain dom = getVariable(position).getDomain();

        for (int i = dom.last(); i >= 0; i = dom.prev(i)) {
            if (dom.value(i) > value || !strict && dom.value(i) == value) {
                dom.remove(i);
                removed = true;
            } else if (ordered) {
                break;
            }
        }

        return removed;
    }

    private boolean removeLt(final int value, final int position) {
        boolean removed = false;
        final Domain dom = getVariable(position).getDomain();

        for (int i = dom.first(); i >= 0; i = dom.next(i)) {
            if (dom.value(i) < value || !strict && dom.value(i) == value) {
                dom.remove(i);
                removed = true;
            } else if (ordered) {
                break;
            }
        }

        return removed;
    }

    @Override
    public boolean revise(RevisionHandler revisator, int reviseCount) {
        if (removeLt(min(1), 0)) {
            if (getVariable(0).getDomainSize() == 0) {
                return false;
            }
            revisator.revised(this, getVariable(0));
        }
        if (removeGt(max(1), 0)) {
            if (getVariable(1).getDomainSize() == 0) {
                return false;
            }
            revisator.revised(this, getVariable(1));
        }
        return true;
    }

    public static boolean generate(final CSPOMConstraint constraint,
            final Problem problem) throws FailedGenerationException {

        final LinkedList<CSPOMVariable> scope = new LinkedList<CSPOMVariable>(
                constraint.getScope());

        final Variable reifiedVariable = problem
                .getSolverVariable(scope.poll());

        if (reifiedVariable.getDomain() == null
                || reifiedVariable.getDomainSize() != 1
                || reifiedVariable.getDomain().value(
                        reifiedVariable.getDomain().first()) != 1) {
            return false;
        }

        final Variable[] solverVariables = ConstraintManager
                .getSolverVariables(scope, problem);

        for (Variable v : solverVariables) {
            if (v.getDomain() == null) {
                return false;
            }
        }
        if ("gt".equals(constraint.getDescription())) {
            problem.addConstraint(new Gt(solverVariables[0],
                    solverVariables[1], true));
        } else if ("ge".equals(constraint.getDescription())) {
            problem.addConstraint(new Gt(solverVariables[0],
                    solverVariables[1], false));
        } else if ("lt".equals(constraint.getDescription())) {
            problem.addConstraint(new Gt(solverVariables[1],
                    solverVariables[0], true));
        } else if ("le".equals(constraint.getDescription())) {
            problem.addConstraint(new Gt(solverVariables[1],
                    solverVariables[0], false));
        } else {
            throw new FailedGenerationException("Unhandled constraint "
                    + constraint);
        }

        return true;
    }
}
