/*
 * Created on 23 mai 08
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package cspfj.constraint.extension;

import cspfj.constraint.AbstractAC3Constraint;
import cspfj.problem.Variable;

public abstract class AbstractExtensionConstraint extends AbstractAC3Constraint
		implements ExtensionConstraint {

	// private static final Logger LOGGER = Logger
	// .getLogger(AbstractExtensionConstraint.class.getName());

	private MatrixManager matrixManager;

	public AbstractExtensionConstraint(MatrixManager matrixManager,
			Variable... scope) {
		super(scope);
		this.matrixManager = matrixManager;
		matrixManager.setTuple(tuple);

	}

	public AbstractExtensionConstraint(String name,
			MatrixManager matrixManager, Variable... scope) {
		super(name, scope);
		this.matrixManager = matrixManager;
		matrixManager.setTuple(tuple);
	}

	@Override
	public MatrixManager getMatrixManager() {
		return matrixManager;
	}

	@Override
	public final boolean removeTuple(int[] tuple) {
		return matrixManager.removeTuple(tuple);
	}

	@Override
	public boolean check() {
		return matrixManager.check();
	}

	public String getType() {
		return super.getType() + " w/ " + matrixManager.getType();
	}

	@Override
	public final int removeTuples(int[] base) {
		int removed = 0;
		tupleManager.setFirstTuple(base);
		do {
			if (removeTuple(this.tuple)) {
				removed++;
			}
		} while (tupleManager.setNextTuple(base));
		return removed;
	}

	public boolean revise(final int position) {
		if (matrixManager.supportCondition(position)) {

			assert !super.revise(position);
			return false;
		}
		return super.revise(position);
	}

	public int getEvaluation(int reviseCount) {

		int size = 1;
		for (Variable v : getScope()) {
			size *= v.getDomainSize();
		}
		return size;
	}

}
