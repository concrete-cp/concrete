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

import cspfj.problem.Variable;

public final class Arc implements Comparable<Arc> {
    private final Constraint constraint;

    private final int cId;

    //private final int id ;
    
    private final Variable variable;

    private final int position;

    public Arc(Constraint constraint, int position) {
        this.constraint = constraint;
        this.cId = constraint.getId();
        this.position = position;
        this.variable = constraint.getInvolvedVariables()[position];
        //this.id = this.cId*maxArity+position ;
    }

    public Variable getVariable() {
        return variable;
    }

    public Constraint getConstraint() {
        return constraint;
    }

    public int getCId() {
        return cId;
    }

    public int getPosition() {
        return position;
    }

    public int compareTo(final Arc arg0) {
        final int compare = variable.compareTo(arg0.getVariable());
        return compare == 0 ? cId - arg0.getCId() : compare;
    }

    public boolean revise(final int level) {
        return constraint.revise(position, level);
    }

    public String toString() {
        return "(" + variable + ", " + constraint + ")";
    }
}
