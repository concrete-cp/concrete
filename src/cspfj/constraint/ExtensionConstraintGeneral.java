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

import cspfj.constraint.TupleListDynamic.LLIterator;
import cspfj.exception.FailedGenerationException;
import cspfj.problem.Variable;

public class ExtensionConstraintGeneral extends AbstractExtensionConstraint {

	private final TupleListDynamic dynamic;

	private final boolean[][] found;

	private static Matrix mayNeedClone(final Matrix matrix)
			throws FailedGenerationException {
		if (matrix instanceof TupleListDynamic) {
			try {
				return matrix.clone();
			} catch (CloneNotSupportedException e) {
				throw new FailedGenerationException(e);
			}
		}
		return matrix;
	}

	public ExtensionConstraintGeneral(final Variable[] scope,
			final Matrix matrix) throws FailedGenerationException {
		super(scope, new MatrixManagerGeneral(scope, mayNeedClone(matrix)),
				true);
		this.dynamic = init(matrix);
		if (this.dynamic == null) {
			found = null;
		} else {
			found = init();
		}

	}

	public ExtensionConstraintGeneral(final Variable[] scope,
			final Matrix matrix, final String name)
			throws FailedGenerationException {
		super(scope, new MatrixManagerGeneral(scope, mayNeedClone(matrix)),
				name, true);
		this.dynamic = init(matrix);
		if (this.dynamic == null) {
			found = null;
		} else {
			found = init();
		}
	}

	private TupleListDynamic init(final Matrix matrix) {
		if (matrix instanceof TupleListDynamic) {
			final TupleListDynamic dynamic = (TupleListDynamic) getMatrix()
					.getMatrix();

			removeTupleCache();
			return dynamic;

		}
		return null;

	}

	private boolean[][] init() {
		if (dynamic == null) {
			int max = 0;
			for (int i = getArity(); --i >= 0;) {
				if (getVariable(i).getDomain().length > max) {
					max = getVariable(i).getDomain().length;
				}
			}
			return new boolean[1][max];
		}
		final boolean[][] found = new boolean[getArity()][];
		for (int i = getArity(); --i >= 0;) {
			found[i] = new boolean[getVariable(i).getDomain().length];
		}
		return found;
	}

	// @Override
	// private boolean findValidTuple(final int variablePosition, final int
	// index,
	// final int level) {
	// if (dynamic == null) {
	// return super.findValidTuple(variablePosition, index);
	// }
	//
	// final Iterator<int[]> itr = dynamic.iterator();
	//
	// while (itr.hasNext()) {
	// final int[] tuple = itr.next();
	//
	// if (tuple[variablePosition] != index) {
	// continue;
	// }
	// if (controlTuplePresence(tuple, variablePosition)) {
	// return true;
	// }
	// itr.remove();
	// dynamic.save(tuple, level);
	//
	// }
	//
	// return false;
	//
	// }

	public void restoreLevel(final int level) {
		if (dynamic != null) {
			dynamic.restore(level);
//			assert dynamic.noRemaining(this);
		}
	}

	public boolean isGlobalRevision() {
		return true;
	}

	public boolean[] revise(final int level) {
		if (dynamic == null) {
			return null;
		}
		int toFind = 0;
		for (int i = getArity(); --i >= 0;) {
			Arrays.fill(found[i], true);

			if (!getVariable(i).isAssigned()) {

				for (int index = getVariable(i).getFirst(); index >= 0; index = getVariable(
						i).getNext(index)) {
					found[i][index] = false;
					toFind++;
				}
			}
		}

		final boolean[] revised = new boolean[getArity()];
		
		final LLIterator itr = dynamic.iterator();

		while (itr.hasNext()) {
			final int[] tuple = itr.next();
			//

//			boolean skip = false;
//			for (int i = getArity(); --i >= 0;) {
//				if (found[i][tuple[i]]) {
//					skip = true;
//					break;
//				}
//			}
//			if (skip) {
//				continue;
//			}
			// if (found[tuple[position]]) {
			// continue;
			// }
			if (controlTuplePresence(tuple)) {
				for (int i = getArity(); --i >= 0;) {
					if (!found[i][tuple[i]]) {
						if (--toFind == 0) {
							return revised;
						}
						found[i][tuple[i]] = true;
					}

				}
			} else {
				itr.remove(level);
			}
		}

		// logger.finer("Revising " + variable + " "
		// + Arrays.toString(variable.getCurrentDomain()) + " against "
		// + this);

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
		for (int i = getArity(); --i >= 0;) {
			if (!getVariable(i).isPresent(tuple[i])) {
				return false;
			}
		}
		return true;
	}

//	@Override
//	public boolean revise(final int position, final int level) {
//		if (dynamic == null) {
//			return super.revise(position, level);
//		}
//		final Variable variable = getVariable(position);
//		assert !variable.isAssigned();
//		final boolean[] found = this.found[0];
//
//		Arrays.fill(found, true);
//
//		int toFind = 0;
//		for (int index = variable.getFirst(); index >= 0; index = variable
//				.getNext(index)) {
//			found[index] = false;
//			toFind++;
//		}
//
//		final LLIterator itr = dynamic.iterator();
//
//		while (itr.hasNext()) {
//			final int[] tuple = itr.next();
//
//			if (found[tuple[position]]) {
//				continue;
//			}
//			if (controlTuplePresence(tuple, position)) {
//				found[tuple[position]] = true;
//				if (--toFind == 0) {
//					return false;
//				}
//			} else {
//				itr.remove(level);
//			}
//		}
//
//		boolean revised = false;
//
//		for (int index = variable.getFirst(); index >= 0; index = variable
//				.getNext(index)) {
//
//			if (!found[index]) {
//				// logger.finer("removing " + index + " from " + variable);
//
//				variable.remove(index, level);
//
//				revised = true;
//				setActive(true);
//
//			}
//
//		}
//
//		return revised;
//	}

	public ExtensionConstraintGeneral deepCopy(
			final Collection<Variable> variables)
			throws CloneNotSupportedException {
		final ExtensionConstraintGeneral constraint = (ExtensionConstraintGeneral) super
				.deepCopy(variables);
		constraint.matrix = matrix.deepCopy(constraint.getScope(),
				constraint.tuple);
		return constraint;
	}

	public String getType() {
		return super.getType() + " w/ " + matrix.getType();
	}

	public void initNbSupports() throws InterruptedException {
		if (dynamic == null) {
			super.initNbSupports();
			return;
		}
		logger.fine("Counting " + this + " supports");
		conflictCounts = false;
		final long size = size();
		for (int p = getArity(); --p >= 0;) {
			initSize[p] = size / getVariable(p).getDomainSize();
		}

		if (Thread.interrupted()) {
			logger.info("Interrupted");
			throw new InterruptedException();
		}
		// logger.fine("Counting supports");

		Arrays.fill(nbMaxConflicts, 0);
		for (int p = getArity(); --p >= 0;) {
			Arrays.fill(nbInitConflicts[p], initSize[p]);
		}

		for (int[] tuple : dynamic) {
			if (Thread.interrupted()) {
				logger.info("Interrupted");
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
