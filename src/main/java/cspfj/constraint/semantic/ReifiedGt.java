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

import cspfj.constraint.AbstractAC3Constraint;
import cspfj.exception.FailedGenerationException;
import cspfj.generator.ConstraintManager;
import cspfj.problem.BitVectorDomain;
import cspfj.problem.Problem;
import cspfj.problem.Variable;
import cspom.constraint.CSPOMConstraint;
import cspom.constraint.FunctionalConstraint;

public final class ReifiedGt extends AbstractAC3Constraint {

    final private boolean strict;

    static {
        ConstraintManager.register("gt", ReifiedGt.class);
        ConstraintManager.register("lt", ReifiedGt.class);
        ConstraintManager.register("ge", ReifiedGt.class);
        ConstraintManager.register("le", ReifiedGt.class);
    }

    public ReifiedGt(final Variable result, final Variable v0,
            final Variable v1, final boolean strict) {
        super(result, v0, v1);
        this.strict = strict;
    }

    @Override
    public boolean check() {
        if (strict) {
            return getValue(0) == (getValue(1) > getValue(2) ? 1 : 0);
        }
        return getValue(0) == (getValue(1) >= getValue(2) ? 1 : 0);
    }

    public static boolean generate(final CSPOMConstraint constraint,
            final Problem problem) throws FailedGenerationException {

        if (!(constraint instanceof FunctionalConstraint)) {
            return false;
        }

        final Variable[] solverVariables = ConstraintManager
                .getSolverVariables(constraint.getScope(), problem);

        if (solverVariables[1].getDomain() == null
                || solverVariables[2].getDomain() == null) {
            return false;
        }

        if (solverVariables[0].getDomain() == null) {
            solverVariables[0].setDomain(new BitVectorDomain(0, 1));
        }

        if ("gt".equals(constraint.getDescription())) {
            problem.addConstraint(new ReifiedGt(solverVariables[0],
                    solverVariables[1], solverVariables[2], true));
        } else if ("ge".equals(constraint.getDescription())) {
            problem.addConstraint(new ReifiedGt(solverVariables[0],
                    solverVariables[1], solverVariables[2], false));
        } else if ("lt".equals(constraint.getDescription())) {
            problem.addConstraint(new ReifiedGt(solverVariables[0],
                    solverVariables[2], solverVariables[1], true));
        } else if ("le".equals(constraint.getDescription())) {
            problem.addConstraint(new ReifiedGt(solverVariables[0],
                    solverVariables[2], solverVariables[1], false));
        } else {
            throw new FailedGenerationException("Unhandled constraint "
                    + constraint);
        }

        return true;
    }
}
