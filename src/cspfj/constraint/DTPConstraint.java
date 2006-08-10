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

public final class DTPConstraint extends AbstractConstraint {

    final private int duration0;

    final private int duration1;

    public DTPConstraint(final Variable[] scope, final int duration0,
            final int duration1) {
        super(scope);
        // System.out.println(scope[0] + "="+i +" <=> "+scope[1]+"="+v);
        this.duration0 = duration0;
        this.duration1 = duration1;
    }

    @Override
    public boolean check() {
        final int value0 = getInvolvedVariables()[0].getDomain()[tuple[0]];
        final int value1 = getInvolvedVariables()[1].getDomain()[tuple[1]];
        return value0 + duration0 < value1 || value1 + duration1 < value0;
    }

    @Override
    public boolean useTupleCache() {
        return true;
    }

}
