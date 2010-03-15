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

import cspfj.constraint.AbstractPVRConstraint;
import cspfj.problem.Domain;
import cspfj.problem.Variable;

public final class DTConstraint extends AbstractPVRConstraint {

    final private int[] duration;

    final private boolean ordered;

    public DTConstraint(final Variable[] scope, final int duration0,
            final int duration1) {
        this(scope, duration0, duration1, false);
    }

    public DTConstraint(final Variable[] scope, final int duration0,
            final int duration1, final boolean ordered) {
        super(scope);
        this.duration = new int[] { duration0, duration1 };
        // System.out.println(this);
        this.ordered = ordered;
    }

    @Override
    public boolean check() {
        final int difference = getValue(0) - getValue(1);

        return (-difference >= duration[0] || difference >= duration[1]);
    }

    public String toString() {
        return "C" + getId() + " (" + getVariable(0) + ":" + duration[0] + ", "
                + getVariable(1) + ":" + duration[1] + ")";
    }

    @Override
    public boolean revise(final int position) {
        final Variable variable = getVariable(position);
        final Variable otherVariable = getVariable(1 - position);

        final Domain domain = variable.getDomain();

        final Domain otherDomain = otherVariable.getDomain();

        final int lBound;
        final int hBound;

        if (ordered) {

            lBound = otherDomain.value(otherDomain.last()) - duration[position];

            hBound = otherDomain.value(otherDomain.first())
                    + duration[1 - position];

        } else {
            int otherMin = otherDomain.value(otherDomain.first());
            int otherMax = otherMin;

            for (int i = otherVariable.getFirst(); i >= 0; i = otherVariable
                    .getNext(i)) {
                final int value = otherDomain.value(i);
                if (value < otherMin) {
                    otherMin = value;
                }
                if (value > otherMax) {
                    otherMax = value;
                }

            }

            lBound = otherMax - duration[position];
            hBound = otherMin + duration[1 - position];
        }

        if (lBound >= hBound) {
            return false;
        }

        boolean filtered = false;

        if (ordered) {
            int i = variable.getFirst();
            while (i >= 0 && domain.value(i) <= lBound) {
                i = variable.getNext(i);
            }
            if (i >= 0 && domain.value(i) < hBound) {
                variable.remove(i);
                filtered = true;
                i = variable.getNext(i);
            }
            while (i >= 0 && domain.value(i) < hBound) {
                variable.remove(i);
                i = variable.getNext(i);
            }
        } else {
            for (int i = variable.getFirst(); i >= 0; i = variable.getNext(i)) {
                if (domain.value(i) > lBound && domain.value(i) < hBound) {
                    variable.remove(i);
                    filtered = true;
                }
            }
        }
        return filtered;
    }

}
