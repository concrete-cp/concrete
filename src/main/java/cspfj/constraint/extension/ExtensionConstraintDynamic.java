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

package cspfj.constraint.extension;

import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.logging.Logger;

import cspfj.constraint.AbstractConstraint;
import cspfj.constraint.DynamicConstraint;
import cspfj.exception.FailedGenerationException;
import cspfj.filter.RevisionHandler;
import cspfj.problem.Variable;
import cspfj.util.BitVector;

public class ExtensionConstraintDynamic extends AbstractConstraint implements
		DynamicConstraint {

	private final MatrixManagerDynamic dynamic;

	private final BitVector[] found;

	private final static Logger logger = Logger
			.getLogger(ExtensionConstraintDynamic.class.getSimpleName());

	public static boolean quick = false;

	public ExtensionConstraintDynamic(final Variable[] scope,
			final TupleSet matrix, final boolean shared)
			throws FailedGenerationException {
		super(scope);
		this.dynamic = new MatrixManagerDynamic(scope, matrix, shared);
		found = initFound();
	}

	public ExtensionConstraintDynamic(final Variable[] scope,
			final TupleSet matrix, final String name, final boolean shared)
			throws FailedGenerationException {
		super(scope, name);
		this.dynamic = new MatrixManagerDynamic(scope, matrix, shared);
		found = initFound();
	}

	private BitVector[] initFound() {

		final BitVector[] found = new BitVector[getArity()];
		for (int i = getArity(); --i >= 0;) {
			found[i] = BitVector.factory(getVariable(i).getDomain().maxSize(),
					false);
		}
		return found;
	}

	public void setLevel(final int level) {
		dynamic.setLevel(level);
	}

	public void restore(final int level) {
		dynamic.restore(level);
	}

	public boolean revise(final RevisionHandler revisator) {
		// logger.fine("Revising "+this);
		int toFind = 0;
		for (int i = getArity(); --i >= 0;) {
			found[i].fill(true);

			if (!getVariable(i).isAssigned()) {
				for (int index = getVariable(i).getFirst(); index >= 0; index = getVariable(
						i).getNext(index)) {
					found[i].clear(index);
					toFind++;
				}
			}
		}

		final Iterator<int[]> itr = dynamic.iterator();

		while (itr.hasNext()) {
			final int[] tuple = itr.next();
			if (tuple == null) {
				continue;
			}
			if (controlTuplePresence(tuple)) {
				for (int i = getArity(); --i >= 0;) {
					if (!quick || !found[i].get(tuple[i])) {
						found[i].set(tuple[i]);
						toFind--;
					}

				}
				if (quick && toFind <= 0) {
					break;
				}
			} else if (!quick) {
				itr.remove();
			}
		}

		for (int i = getArity(); --i >= 0;) {
			final Variable variable = getVariable(i);

			boolean rev = false;
			final BitVector found = this.found[i];
			for (int index = found.lastClearBit(); index >= 0; index = found
					.prevClearBit(index)) {
				variable.remove(index);
				rev = true;
			}

			if (rev) {
				if (variable.getDomainSize() <= 0) {
					return false;
				}
				revisator.revised(this, variable);
				setActive(true);
			}
		}

		return true;
	}

	private boolean controlTuplePresence(final int[] tuple) {
		if (tuple.length != getArity()) {
			return false;
		}
		for (int i = getArity(); --i >= 0;) {
			if (tuple == null || !getVariable(i).isPresent(tuple[i])) {
				return false;
			}
		}
		return true;
	}

	public ExtensionConstraintDynamic deepCopy(
			final Collection<Variable> variables)
			throws CloneNotSupportedException {
		final ExtensionConstraintDynamic constraint = (ExtensionConstraintDynamic) super
				.deepCopy(variables);
		// constraint.matrix = matrix.deepCopy(constraint.getScope(),
		// constraint.tuple);
		return constraint;
	}

	public void initNbSupports() throws InterruptedException {
		// logger.fine("Counting " + this + " supports");

		final long[][] nbInitConflicts = new long[getArity()][];
		for (int i = getArity(); --i >= 0;) {
			nbInitConflicts[i] = new long[getScope()[i].getDomain().maxSize()];
		}
		final long[] nbMaxConflicts = new long[getArity()];

		final long size = currentSize();
		if (size < 0) {
			return;
		}

		Arrays.fill(nbMaxConflicts, 0);
		for (int p = getArity(); --p >= 0;) {
			Arrays.fill(nbInitConflicts[p], size
					/ getVariable(p).getDomainSize());
		}

		for (int[] tuple : dynamic) {
			if (Thread.interrupted()) {
				logger.fine("Interrupted");
				throw new InterruptedException();
			}
			for (int p = getArity(); --p >= 0;) {
				nbInitConflicts[p][tuple[p]]--;
			}
		}

		for (int p = getArity(); --p >= 0;) {
			final Variable variable = getVariable(p);
			long max = 0;
			final long[] hereConflicts = nbInitConflicts[p];
			for (int i = variable.getFirst(); i != -1; i = variable.getNext(i)) {
				if (max < hereConflicts[i]) {
					max = hereConflicts[i];
				}
			}
			nbMaxConflicts[p] = max;
		}
		this.nbInitConflicts = nbInitConflicts;
		this.nbMaxConflicts = nbMaxConflicts;
	}

	@Override
	public boolean check() {
		return dynamic.check();
	}

	@Override
	public boolean removeTuple(int[] tuple) {
		if (dynamic.removeTuple(tuple)) {
			addConflict(tuple);
			return true;
		}
		return false;
	}

	private static boolean match(int[] tuple, int[] base) {
		assert tuple.length == base.length;
		for (int i = tuple.length; --i >= 0;) {
			if (base[i] >= 0 && base[i] != tuple[i]) {
				return false;
			}
		}
		return true;
	}

	@Override
	public int removeTuples(int[] base) {
		int removed = 0;
		for (final Iterator<int[]> itr = dynamic.iterator(); itr.hasNext();) {
			final int[] currentTuple = itr.next();
			if (match(currentTuple, base)) {
				itr.remove();
				removed++;
			}
		}
		return removed;
	}
	
	@Override
	public boolean positive() {
		return true;
	}
}
