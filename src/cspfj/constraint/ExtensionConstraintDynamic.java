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
import java.util.Collection;
import java.util.logging.Logger;

import cspfj.constraint.MatrixManagerDynamic.LLIterator;
import cspfj.exception.FailedGenerationException;
import cspfj.problem.Variable;

public class ExtensionConstraintDynamic extends AbstractExtensionConstraint {

	private final MatrixManagerDynamic dynamic;

	private final boolean[][] found;

	private final static Logger logger = Logger
			.getLogger(ExtensionConstraintDynamic.class.getSimpleName());

	public ExtensionConstraintDynamic(final Variable[] scope,
			final TupleArray matrix) throws FailedGenerationException {
		super(scope, new MatrixManagerDynamic(scope, matrix), false);
		this.dynamic = (MatrixManagerDynamic) this.matrix;
		found = initFound();
	}

	public ExtensionConstraintDynamic(final Variable[] scope,
			final TupleArray matrix, final String name)
			throws FailedGenerationException {
		super(scope, new MatrixManagerDynamic(scope, matrix), name, false);
		this.dynamic = (MatrixManagerDynamic) this.matrix;
		found = initFound();
	}

	private boolean[][] initFound() {

		final boolean[][] found = new boolean[getArity()][];
		for (int i = getArity(); --i >= 0;) {
			found[i] = new boolean[getVariable(i).getDomain().length];
		}
		return found;
	}

	public void restore(final int level) {
		dynamic.restore(level);
	}

	public boolean[] revise(final int level) {
		// logger.fine("Revising "+this);
		// int toFind = 0;
		for (int i = getArity(); --i >= 0;) {
			Arrays.fill(found[i], true);

			if (!getVariable(i).isAssigned()) {
				for (int index = getVariable(i).getFirst(); index >= 0; index = getVariable(
						i).getNext(index)) {
					found[i][index] = false;
					// toFind++;
				}
			}
		}

		final boolean[] revised = new boolean[getArity()];

		final LLIterator itr = dynamic.iterator();

		while (itr.hasNext()) {
			final int[] tuple = itr.next();

			if (controlTuplePresence(tuple)) {
				for (int i = getArity(); --i >= 0;) {
					found[i][tuple[i]] = true;
				}
			} else {
				itr.remove(level);
			}
		}

		for (int i = getArity(); --i >= 0;) {
			final Variable variable = getVariable(i);

			boolean rev = false;
			final boolean[] found = this.found[i];
			for (int index = variable.getFirst(); index >= 0; index = variable
					.getNext(index)) {

				if (!found[index]) {
					// logger.finer("removing " + index + " from " + variable);

					variable.remove(index, level);

					rev = true;

				}

			}
			if (rev) {
				revised[i] = true;
				setActive(true);
			}
		}

		return revised;
	}

	private boolean controlTuplePresence(final int[] tuple) {
//		if (tuple.length != getArity()) {
//			return false;
//		}
		for (int i = getArity(); --i >= 0;) {
			if (!getVariable(i).isPresent(tuple[i])) {
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
		constraint.matrix = matrix.deepCopy(constraint.getScope(),
				constraint.tuple);
		return constraint;
	}



	public void initNbSupports() throws InterruptedException {
		// logger.fine("Counting " + this + " supports");
		conflictCounts = false;
		final long size = size();
		for (int p = getArity(); --p >= 0;) {
			initSize[p] = size / getVariable(p).getDomainSize();
		}

		if (Thread.interrupted()) {
			logger.fine("Interrupted");
			throw new InterruptedException();
		}
		// logger.fine("Counting supports");

		Arrays.fill(nbMaxConflicts, 0);
		for (int p = getArity(); --p >= 0;) {
			Arrays.fill(nbInitConflicts[p], initSize[p]);
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
			final long[] nbInitConflicts = this.nbInitConflicts[p];
			for (int i = variable.getFirst(); i != -1; i = variable.getNext(i)) {
				if (max < nbInitConflicts[i]) {
					max = nbInitConflicts[i];
				}
			}
			nbMaxConflicts[p] = max;
		}
		conflictCounts = true;
	}
}
