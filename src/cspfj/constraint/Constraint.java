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

import java.util.List;

import cspfj.problem.Variable;
import cspfj.util.Arc;

public interface Constraint {
    boolean revise(final Variable variable, final int level);

    boolean revise(final int position, final int level) ;
    
    void increaseWeight();

    int getWeight();

    float getFreedomDegree();

    int getId();

    Variable[] getInvolvedVariables();

    Arc[] getArcs();

    int getNbTuples(final Variable variable, final int index);

    boolean check();

    boolean checkFirst();

    boolean checkFirstWith(final Variable variable, final int index);

    void setWeight(int weight);

    boolean findValidTuple(final Variable variable, final int index);

    boolean isInvolved(final Variable variable);

    int getArity();

    boolean useTupleCache();

    boolean removeTuple(final List<Variable> scope, final List<Integer> tuple);

    // boolean revise(int level);
}
