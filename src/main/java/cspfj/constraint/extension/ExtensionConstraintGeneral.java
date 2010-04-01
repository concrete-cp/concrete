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

package cspfj.constraint.extension;

import cspfj.constraint.AbstractAC3Constraint;
import cspfj.constraint.DynamicConstraint;
import cspfj.problem.Variable;

public final class ExtensionConstraintGeneral extends AbstractAC3Constraint
        implements ExtensionConstraint, DynamicConstraint {

    private MatrixManager matrixManager;

    public ExtensionConstraintGeneral(final Matrix matrix,
            final boolean shared, final Variable... scope) {
        super(scope);
        this.matrixManager = new MatrixManagerGeneral(scope, matrix, shared,
                getTuple());
    }

    public ExtensionConstraintGeneral(final String name, final Matrix matrix,
            final boolean shared, final Variable... scope) {
        super(name, scope);
        this.matrixManager = new MatrixManagerGeneral(scope, matrix, shared,
                getTuple());
    }

    @Override
    public MatrixManager getMatrixManager() {
        return matrixManager;
    }

    @Override
    public boolean removeTuple(final int[] tuple) {
        return matrixManager.removeTuple(tuple);
    }

    @Override
    public boolean check() {
        return matrixManager.check();
    }

    public String getType() {
        return super.getType() + " w/ " + matrixManager.getType();
    }

    @Override
    public int removeTuples(final int[] base) {
        int removed = 0;
        tupleManager.setFirstTuple(base);
        do {
            if (removeTuple(this.tuple)) {
                removed++;
            }
        } while (tupleManager.setNextTuple(base));
        return removed;
    }

    @Override
    public boolean revise(final int position) {
        if (matrixManager.supportCondition(position)) {

            assert !super.revise(position);
            return false;
        }
        return super.revise(position);
    }

    @Override
    public int getEvaluation(final int reviseCount) {

        int size = 1;
        for (Variable v : getScope()) {
            size *= v.getDomainSize();
        }
        return size;
    }
}
