/*
 * Created on 23 mai 08
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package cspfj.constraint;

import cspfj.problem.Variable;

public abstract class AbstractExtensionConstraint extends AbstractConstraint
		implements ExtensionConstraint {

	protected AbstractMatrixManager matrix;

	public AbstractExtensionConstraint(Variable[] scope, AbstractMatrixManager matrix) {
		super(scope);
		this.matrix = matrix;
		matrix.setTuple(tuple);
	}
	
	public AbstractExtensionConstraint(Variable[] scope, AbstractMatrixManager matrix, String name) {
		super(scope, name);
		this.matrix = matrix;
		matrix.setTuple(tuple);
	}

	@Override
	public AbstractMatrixManager getMatrix() {
		return matrix;
	}


	@Override
	public boolean removeTuple() {
		return matrix.removeTuple();
	}
	
	@Override
	public boolean check() {
		return matrix.check();
	}
}
