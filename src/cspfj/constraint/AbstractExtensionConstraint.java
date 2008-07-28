/*
 * Created on 23 mai 08
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package cspfj.constraint;

import cspfj.problem.Variable;

public abstract class AbstractExtensionConstraint extends AbstractAC3Constraint
		implements ExtensionConstraint, DynamicConstraint {

	protected MatrixManager matrix;

	public AbstractExtensionConstraint(Variable[] scope, MatrixManager matrix) {
		super(scope);
		this.matrix = matrix;
		matrix.setTuple(tuple);
	}

	public AbstractExtensionConstraint(Variable[] scope, MatrixManager matrix,
			String name) {
		super(scope, name);
		this.matrix = matrix;
		matrix.setTuple(tuple);
	}

	@Override
	public MatrixManager getMatrix() {
		return matrix;
	}

	@Override
	public boolean removeTuple(int[] tuple) {
		return matrix.removeTuple(tuple);
	}

	@Override
	public boolean check() {
		return matrix.check();
	}

	public String getType() {
		return super.getType() + " w/ " + matrix.getType();
	}
}
