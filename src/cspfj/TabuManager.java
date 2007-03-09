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

import java.util.Arrays;

import cspfj.problem.Problem;
import cspfj.problem.Variable;

public final class TabuManager {
	private boolean[][] tabus;

	private int[] t1;

	private int[] t2;

	private int head;

	private final int size;

	public TabuManager(final Problem problem, final int size) {
		this.size = size;
		if (size < 1) {
			return;
		}
		tabus = new boolean[problem.getNbVariables()][problem
				.getMaxDomainSize()];
		t1 = new int[size];
		t2 = new int[size];
		clean();
	}

	public void push(final int vid, final int index) {
		// assert tabus[variable.getId()][index] != true : variable + " " +
		// index + " already tabu";

		tabus[t1[head]][t2[head]] = false;
		tabus[vid][index] = true;

		t1[head] = vid;
		t2[head] = index;
		head = (head + 1) % size;

		// System.out.println( variable + " " + index + " tabu");
	}

	public boolean isTabu(final int vid, final int index) {
		return tabus != null && tabus[vid][index];
	}

	public void clean() {
		for (boolean[] tabu : tabus) {
			Arrays.fill(tabu, false);
		}
		Arrays.fill(t1, 0);
		Arrays.fill(t2, 0);
		head = 0;
	}

	public int getSize() {
		return size;
	}
}
