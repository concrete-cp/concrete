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

package cspfj.ls;

import java.util.Arrays;

import cspfj.problem.Problem;

public final class TabuManager {
	private final int[][] tabus;

	private final int size;

	public TabuManager(final Problem problem, final int size) {
		this.size = size;
		tabus = new int[problem.getNbVariables()][problem.getMaxDomainSize()];
		 clean();
	}

	public void push(final int vid, final int index, final int nbIt) {
		tabus[vid][index] = nbIt + size;
	}

	public boolean isTabu(final int vid, final int index, final int nbIt) {
		return tabus[vid][index] >= nbIt;
	}

	public void clean() {
		for (int[] tabu:tabus) {
			Arrays.fill(tabu, -1);
		}
	}

	public int getSize() {
		return size;
	}
}