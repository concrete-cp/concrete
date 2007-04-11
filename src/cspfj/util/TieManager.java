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

public final class TieManager {

	private int bestValue;

	private int bestEvaluation;

	private int nbTies;

	private final Random random;

	public TieManager(Random random) {
		super();

		this.random = random;
		clear();
	}

	public void clear() {
		bestValue = -1;
		bestEvaluation = Integer.MAX_VALUE;
		nbTies = 1;
	}

	public boolean newValue(final int value, final int evaluation) {
		if (newValue(evaluation)) {
			this.bestValue = value;
			return true ;
		}
		return false;
	}

	public boolean newValue(final int evaluation) {
		if (evaluation == bestEvaluation) {
			return random.nextFloat() * nbTies++ < 1;
		}

		if (evaluation < bestEvaluation) {
			nbTies = 2;
			this.bestEvaluation = evaluation;
			return true;
		}

		return false;
	}

	public int getBestValue() {
		return bestValue;
	}

	public int getBestEvaluation() {
		return bestEvaluation;
	}

}
