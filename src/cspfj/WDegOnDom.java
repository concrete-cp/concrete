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

package cspfj;

import java.util.Random;

import cspfj.problem.Problem;
import cspfj.problem.Variable;
import cspfj.util.TieManager;

public final class WDegOnDom {

    private final Problem problem;

    private final Random random;

    private final static TieManager<Variable, Float> tieManager = new TieManager<Variable, Float>(
            null, 0F);

    public WDegOnDom(final Problem prob, final Random random) {
        super();
        problem = prob;
        this.random = random;

    }

    public Variable selectVariable() {
        final TieManager<Variable, Float> tieManager = WDegOnDom.tieManager;

        for (Variable v : problem.getVariables()) {
            if (v.isAssigned()) {
                continue;
            }

            if (v.getDomainSize() == 1) { // || !v.isSelectable()) {
                tieManager.newValue(v, 0F, random);
            } else {

                tieManager
                        .newValue(v, -v.getWDeg() / v.getDomainSize(), random);
            }

            // System.out.print(est);
            // System.out.print(" ");
        }
        // System.out.println("Selected : " + bestVariable + "(" + best + ")");

        final Variable bestVariable = tieManager.getBestValue();
        tieManager.clear();

        return bestVariable;
    }

}
