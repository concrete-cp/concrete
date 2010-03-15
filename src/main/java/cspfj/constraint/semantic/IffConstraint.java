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
import cspfj.problem.Variable;

public final class IffConstraint extends AbstractPVRConstraint {

    final private int indexI;

    final private int indexV;

    final private int valueI;

    final private int valueV;

    public IffConstraint(final Variable[] scope, final int i, final int v) {
        super(scope);
        // System.out.println(scope[0] + "="+i +" <=> "+scope[1]+"="+v);
        this.valueI = i;
        this.indexI = getVariable(0).getDomain().index(i);
        this.valueV = v;
        this.indexV = getVariable(1).getDomain().index(v);

    }

    @Override
    public boolean check() {
        return (getVariable(0).getDomain().value(tuple[0]) == valueI) == (getVariable(
                1).getDomain().value(tuple[1]) == valueV);
    }

    @Override
    public boolean revise(final int posVar1) {
        final Variable var1 = getVariable(posVar1);

        final int posVar2 = 1 - posVar1;
        final Variable var2 = getVariable(posVar2);

        final int value1;
        final int index1;
        final int index2;
        if (posVar1 == 0) {
            value1 = valueI;
            index1 = indexI;
            index2 = indexV;
        } else {
            value1 = valueV;
            index1 = indexV;
            index2 = indexI;
        }

        boolean revised = false;

        if (var2.getDomainSize() == 1 && var2.getFirst() == index2) {
            revised = true;
            var1.makeSingleton(value1);
        }

        if (!var2.isPresent(index2) && var1.isPresent(index1)) {
            revised = true;
            var1.remove(index1);
        }
        return revised;

    }

}
