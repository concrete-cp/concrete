/*
 * Created on 19 mars 2007
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package cspfj.constraint;

import java.util.List;

import cspfj.problem.Variable;

public class TupleManager {

	private final Constraint constraint;

	private final int[] tuple;

	private final int arity;

	public TupleManager(Constraint constraint, int[] tuple) {
		super();
		this.constraint = constraint;
		this.tuple = tuple;
		arity = constraint.getArity();
	}

	public void setRealTuple(final List<Variable> scope,
			final List<Integer> tuple) {
		final int[] realTuple = this.tuple;
		final Variable[] involvedVariables = constraint.getInvolvedVariables();

		for (int i = arity; --i >= 0;) {
			for (int j = arity; --j >= 0;) {
				if (scope.get(i) == involvedVariables[j]) {
					realTuple[j] = tuple.get(i);
					break;
				}
			}
		}
	}

	public void setFirstTuple() {
		for (int position = arity; --position >= 0;) {
			tuple[position] = constraint.getInvolvedVariables()[position]
					.getFirst();
		}
	}

	public boolean setNextTuple() {
		final int[] tuple = this.tuple;
		final Variable[] involvedVariables = constraint.getInvolvedVariables();
		for (int i = arity; --i >= 0;) {
			final int index = involvedVariables[i].getNext(tuple[i]);

			if (index < 0) {
				tuple[i] = involvedVariables[i].getFirst();
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
				tuple[position] = constraint.getInvolvedVariables()[position].getFirst();
			}
		}

	}

	public boolean setNextTuple(final int fixedVariablePosition) {
		final int[] tuple = this.tuple;
		final Variable[] involvedVariables = constraint.getInvolvedVariables();
		for (int i = arity; --i >= 0;) {
			if (i == fixedVariablePosition) {
				continue;
			}

			final int index = involvedVariables[i].getNext(tuple[i]);

			if (index < 0) {
				tuple[i] = involvedVariables[i].getFirst();
			} else {
				tuple[i] = index;
				return true;
			}
		}
		return false;
	}
}
