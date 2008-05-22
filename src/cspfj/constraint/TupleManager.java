/*
 * Created on 19 mars 2007
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package cspfj.constraint;

import java.util.Arrays;
import java.util.List;

import cspfj.problem.Variable;

public final class TupleManager {

	private final AbstractConstraint constraint;

	private final int[] tuple;

	private final int arity;

	public TupleManager(final AbstractConstraint constraint, final int[] tuple) {
		super();
		this.constraint = constraint;
		this.tuple = tuple;
		arity = constraint.getArity();
	}

	public void setRealTuple(final List<Variable> scope,
			final List<Integer> tuple) {
		final int[] realTuple = this.tuple;

		for (int i = arity; --i >= 0;) {
			for (int j = arity; --j >= 0;) {
				if (scope.get(i) == constraint.getVariable(j)) {
					realTuple[j] = tuple.get(i);
					break;
				}
			}
		}
	}

	public void setFirstTuple() {
		for (int position = arity; --position >= 0;) {
			tuple[position] = constraint.getVariable(position).getFirst();
		}
	}

	public boolean setNextTuple() {
		final int[] tuple = this.tuple;
		for (int i = arity; --i >= 0;) {
			final int index = constraint.getVariable(i).getNext(tuple[i]);

			if (index < 0) {
				tuple[i] = constraint.getVariable(i).getFirst();
			} else {
				tuple[i] = index;
				return true;
			}
		}
		return false;
	}

	public void setFirstTuple(final int variablePosition, final int index) {
		for (int position = arity; --position >= 0;) {
			if (position == variablePosition) {
				tuple[position] = index;
			} else {
				tuple[position] = constraint.getVariable(position).getFirst();
			}
		}

	}

	public boolean setNextTuple(final int fixedVariablePosition) {
		final int[] tuple = this.tuple;

		for (int i = arity; --i >= 0;) {
			if (i == fixedVariablePosition) {
				continue;
			}

			final int index = constraint.getVariable(i).getNext(tuple[i]);

			if (index < 0) {
				tuple[i] = constraint.getVariable(i).getFirst();
			} else {
				tuple[i] = index;
				return true;
			}
		}
		return false;
	}

	public void setTuple(final int[] tpl) {
		System.arraycopy(tpl, 0, tuple, 0, arity);
		assert allPresent();
	}

	private boolean allPresent() {
		for (int i = arity; --i >= 0;) {
			if (!constraint.getVariable(i).isPresent(tuple[i])) {
				return false;
			}
		}
		return true;
	}

	public boolean setTupleAfter(final int[] tpl, final int fixed) {
		final int[] tuple = this.tuple;
		// System.arraycopy(tpl, 0, tuple, 0, arity);

		tuple[fixed] = tpl[fixed];
		int changed = arity;

		for (int pos = 0; pos < arity; pos++) {
			if (pos == fixed) {
				continue;
			}
			if (changed == arity) {
				tuple[pos] = tpl[pos];
			}

			final Variable variable = constraint.getVariable(pos);

			if (pos > changed) {
				tuple[pos] = variable.getFirst();
			} else {

				int index = tuple[pos];

				while (index >= 0 && !variable.isPresent(index)) {
					changed = pos;
					index = variable.getNext(index);
				}

				if (index < 0) {
					pos--;
					if (pos == fixed) {
						pos--;
					}
					if (pos < 0) {
						return false;
					}
					tuple[pos] = variable.getNext(tuple[pos]);
					changed = pos;
					pos--;
				} else {
					tuple[pos] = index;
				}
			}
		}

		assert allPresent() : Arrays.toString(tpl) + " -> "
				+ Arrays.toString(tuple);

		return true;
	}

	public boolean setPrevTuple(final int fixedVariablePosition) {
		final int[] tuple = this.tuple;
		for (int i = arity; --i >= 0;) {
			if (i == fixedVariablePosition) {
				continue;
			}

			final int index = constraint.getVariable(i).getPrev(tuple[i]);

			if (index < 0) {
				tuple[i] = constraint.getVariable(i).getLast();
			} else {
				tuple[i] = index;
				return true;
			}
		}
		return false;
	}

}
