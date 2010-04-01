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

import cspfj.constraint.AbstractConstraint;
import cspfj.filter.RevisionHandler;
import cspfj.problem.Domain;
import cspfj.problem.Variable;

public final class Gt extends AbstractConstraint {

    private final boolean strict;

    public Gt(final Variable v0, final Variable v1, final boolean strict) {
        super(v0, v1);
        this.strict = strict;
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
        return dom.value(dom.first());
    }

    private int max(final int position) {
        final Domain dom = getVariable(position).getDomain();
        return dom.value(dom.last());
    }

    private boolean removeGt(final int value, final int position) {
        boolean removed = false;
        final Domain dom = getVariable(position).getDomain();

        for (int i = dom.last(); i >= 0; i = dom.prev(i)) {
            if (dom.value(i) > value || !strict && dom.value(i) == value) {
                dom.remove(i);
                removed = true;
            } else {
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
            } else {
                break;
            }
        }

        return removed;
    }

    @Override
    public boolean revise(final RevisionHandler revisator, final int reviseCount) {
        if (removeLt(min(1), 0)) {
            if (getVariable(0).getDomainSize() == 0) {
                return false;
            }
            revisator.revised(this, getVariable(0));
        }
        if (removeGt(max(0), 1)) {
            if (getVariable(1).getDomainSize() == 0) {
                return false;
            }
            revisator.revised(this, getVariable(1));
        }
        return true;
    }

    public String toString() {
        if (strict) {
            return getVariable(0) + " > " + getVariable(1);
        }
        return getVariable(0) + " >= " + getVariable(1);
    }

    @Override
    public int getEvaluation(final int reviseCount) {
        return Math.max(getVariable(0).getDomainSize(), getVariable(1)
                .getDomainSize());
    }
}
