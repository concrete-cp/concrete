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

package cspfj.util;

import java.util.Random;

public final class TieManager<V, E extends Comparable<E>> {

    private V bestValue;

    private final V noValue;

    private final E maxEvaluation;

    private E bestEvaluation;

    private int nbTies;

    public TieManager(V noValue, E maxValue) {
        super();
        this.maxEvaluation = maxValue;
        this.noValue = noValue;
        clear();
    }

    public void clear() {
        bestValue = noValue;
        bestEvaluation = maxEvaluation;
        nbTies = 1;
    }

    public boolean newValue(final V value, final E evaluation,
            final Random random) {
        final int compare = evaluation.compareTo(this.bestEvaluation);

        if (compare == 0 && random.nextFloat() * nbTies++ < 1) {
            this.bestValue = value;
            return true;
        }

        if (compare < 0) {
            nbTies = 1;
            this.bestValue = value;
            this.bestEvaluation = evaluation;
            return true;
        }

        return false;
    }

    public V getBestValue() {
        return bestValue;
    }

    public E getBestEvaluation() {
        return bestEvaluation;
    }

}
