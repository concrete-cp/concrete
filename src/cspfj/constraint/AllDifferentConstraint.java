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

import java.util.Arrays;
import java.util.BitSet;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Map;

import cspfj.problem.Variable;
import cspfj.util.Heap;

public final class AllDifferentConstraint extends AbstractConstraint {

	private final BitSet union;

	private final int min;

	// private final static Logger logger = Logger
	// .getLogger("cspfj.constraint.AllDifferentConstraint");

	public AllDifferentConstraint(final Variable[] scope, final String name) {
		super(scope, name);

		int minVal = Integer.MAX_VALUE;
		for (Variable v : scope) {
			for (int i : v.getDomain()) {
				if (i < minVal) {
					minVal = i;
				}
			}
		}
		union = new BitSet();
		min = minVal;
	}

	@Override
	public boolean check() {
		final BitSet union = this.union;
		union.clear();
		final int min = this.min;

		final int[] tuple = this.tuple;
		for (int i = getArity(); --i >= 0;) {
			final int value = getVariable(i).getDomain()[tuple[i]];
			if (union.get(value - min)) {
				return false;
			}
			union.set(value - min);
		}
		return true;
	}

	public boolean revise(final int level, final boolean[] revised) {
		final Heap<Interval> queue = new Heap<Interval>(
				new Comparator<Interval>() {
					@Override
					public int compare(Interval arg0, Interval arg1) {
						return arg0.getMax() - arg1.getMax();
					}
				}, new Interval[getArity()]);

		final Interval[] domains = new Interval[getArity()];

		int highest = Integer.MIN_VALUE;
		for (int i = getArity(); --i >= 0;) {
			final Interval itv = new Interval(getVariable(i), i);
			domains[i] = itv;
			if (itv.getMax() > highest) {
				highest = itv.getMax();
			}

		}

		Arrays.sort(domains, new Comparator<Interval>() {
			@Override
			public int compare(Interval arg0, Interval arg1) {
				return arg0.getMin() - arg1.getMin();
			}
		});

		final int lowest = domains[0].getMin();

		int minPointer = 0;

		int[] match = new int[highest - lowest + 1];
		Arrays.fill(match, -1);

		for (int j = lowest; j <= highest
				&& (!queue.isEmpty() || minPointer < domains.length); j++) {
			if (queue.isEmpty() && j < domains[minPointer].getMin()) {
				j = domains[minPointer].getMin();
			}
			while (minPointer < domains.length
					&& domains[minPointer].getMin() <= j) {
				queue.add(domains[minPointer++]);
			}

			final Interval itv = queue.pollFirst();

			if (j <= itv.getMax()) {
				match[j - lowest] = itv.getPosition();
			} else {
				return false;
			}
		}

		System.out.println(Arrays.toString(match));

		return true;
	}

	// public boolean revise(final int position, final int level) {
	// final Variable variable = getVariable(position);
	// assert !variable.isAssigned();
	//
	// final boolean[] union = this.union;
	// final int min = this.min;
	//
	// Arrays.fill(union, false);
	// int nbVal = 0;
	//
	// boolean revised = false;
	//
	// for (int checkPos = getArity(); --checkPos >= 0;) {
	// final Variable checkedVariable = getVariable(checkPos);
	// for (int i = checkedVariable.getFirst(); i >= 0; i = checkedVariable
	// .getNext(i)) {
	//
	// if (!union[checkedVariable.getDomain()[i] - min]) {
	// union[checkedVariable.getDomain()[i] - min] = true;
	// nbVal++;
	// }
	//
	// }
	//
	// if (position != checkPos && checkedVariable.getDomainSize() == 1) {
	// final int index = variable
	// .index(checkedVariable.getDomain()[checkedVariable
	// .getFirst()]);
	// if (index >= 0 && variable.isPresent(index)) {
	// variable.remove(index, level);
	// revised = true;
	// setActive(true);
	// }
	// }
	// }
	//
	// if (nbVal < getArity()) {
	// variable.empty(level);
	// setActive(true);
	// return true;
	// }
	//
	// return revised;
	// }

	@Override
	public boolean isSlow() {
		return true;
	}

	private static class Interval {
		private final int min;

		private final int max;

		private final int position;

		public Interval(final Variable variable, final int position) {
			min = variable.getDomain()[variable.getFirst()];
			max = variable.getDomain()[variable.getLast()];
			this.position = position;
		}

		public int getMin() {
			return min;
		}

		public int getMax() {
			return max;
		}

		public int getPosition() {
			return position;
		}

		public String toString() {
			return "[" + min + ", " + max + "]";
		}
	}

	public static void main(String[] args) {
		final Variable[] variables = { new Variable(new int[] { 3, 4 }),
				new Variable(new int[] { 7 }),
				new Variable(new int[] { 2, 3, 4, 5 }),
				new Variable(new int[] { 2, 3, 4, 5, 6, 7 }),
				new Variable(new int[] { 1, 2, 3 }),
				new Variable(new int[] { 3, 4 })

		};
		final Constraint allDiff = new AllDifferentConstraint(variables, "");

		final boolean[] revised = new boolean[variables.length];
		System.out.println(allDiff.revise(0, revised));

	}
}
