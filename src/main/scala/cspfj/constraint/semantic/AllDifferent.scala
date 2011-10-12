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

import java.util.Arrays
import java.util.Deque
import java.util.LinkedList
import cspfj.filter.RevisionHandler
import cspfj.problem.Variable
import cspfj.util.BitVector;
import cspfj.constraint.AbstractConstraint
import cspfj.constraint.ArcGrainedConstraint

 final class AllDifferent(scope: Variable*) extends AbstractConstraint(null, scope.toArray) 
 with ArcGrainedConstraint {
   
   val offset = scope map { _.domain.allValues.min } min
   val max = scope map { _.domain.allValues.max } max
   val union = BitVector.newBitVector(max-offset+1, false)

   var queue: List[Variable] = Nil
   val priority:Float = arity*arity

    def check:Boolean = {
        union.fill(false);
        for (v<-scope.zip(tuple).map(t=>t._1.domain.value(t._2)) {
          if (union.get(v - offset)) {
            return false
          }
          union.set(v - offset)
        }
        true;
    }

    private boolean filter(final Variable checkedVariable, final int value,
            final RevisionHandler revisator) {
        for (Variable v : getScope()) {
            if (v == checkedVariable) {
                continue;
            }
            final int index = v.getDomain().index(value);
            if (index >= 0 && v.isPresent(index)) {
                v.remove(index);
                if (v.getDomainSize() < 1) {
                    return true;
                } else if (v.getDomainSize() == 1) {
                    queue.offer(v);
                }
                revisator.revised(this, v);
            }

        }
        return false;
    }

    @Override
    public boolean revise(final RevisionHandler revisator, final int reviseCount) {
        queue.clear();
        for (int pos = getArity(); --pos >= 0;) {
            if (getRemovals(pos) >= reviseCount
                    && getVariable(pos).getDomainSize() == 1) {
                queue.offer(getVariable(pos));
            }
        }

        while (!queue.isEmpty()) {
            final Variable checkedVariable = queue.poll();
            final int value = checkedVariable.getDomain().value(
                    checkedVariable.getFirst());

            if (filter(checkedVariable, value, revisator)) {
                return false;
            }
        }

        union.fill(false);
        int size = 0;
        for (Variable v : getScope()) {
            for (int i = v.getFirst(); i >= 0; i = v.getNext(i)) {
                if (union.set(v.getDomain().value(i) - offset)
                        && ++size >= getArity()) {
                    return true;

                }
                assert size == union.cardinality();
            }
        }

        return false;
    }

    public String toString() {
        return "allDifferent" + Arrays.toString(getScope());
    }

    @Override
    public float getEvaluation() {
        return priority;
    }
}
