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

import cspfj.constraint.AbstractPVRConstraint;
import cspfj.problem.Variable;

public final class ExtensionConstraint2D extends AbstractPVRConstraint
        implements ExtensionConstraint {

    private static final int GAIN_OVER_GENERAL = 10;

    private final MatrixManager2D matrixManager;

    public ExtensionConstraint2D(final Variable[] scope, final Matrix2D matrix,
            final boolean shared) {
        super(scope);
        this.matrixManager = new MatrixManager2D(scope, matrix, shared,
                getTuple());
    }

    public ExtensionConstraint2D(final Variable[] scope, final Matrix2D matrix,
            final String name, final boolean shared) {
        super(name, scope);
        this.matrixManager = new MatrixManager2D(scope, matrix, shared,
                getTuple());
    }

    public int getEvaluation(final int reviseCount) {
        return getVariable(0).getDomainSize() * getVariable(1).getDomainSize()
                / GAIN_OVER_GENERAL;
    }

    @Override
    public boolean revise(final int position) {
        if (matrixManager.supportCondition(position)) {
            return false;
        }

        final Variable variable = getVariable(position);

        // assert !variable.isAssigned();

        boolean revised = false;

        for (int index = variable.getFirst(); index >= 0; index = variable
                .getNext(index)) {

            if (!matrixManager.hasSupport(position, index)) {
                variable.remove(index);
                revised = true;
            }

        }

        return revised;
    }

    @Override
    public boolean removeTuple(final int[] tuple) {
        return matrixManager.removeTuple(tuple);
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
    public MatrixManager2D getMatrixManager() {
        return matrixManager;
    }

    @Override
    public boolean check() {
        return matrixManager.check();
    }

    @Override
    public String toString() {
        return "ext2d(" + getVariable(0) + ", " + getVariable(1) + ")";
    }
}
