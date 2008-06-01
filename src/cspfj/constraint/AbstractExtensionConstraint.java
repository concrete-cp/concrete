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

	protected MatrixManager matrix;

	public AbstractExtensionConstraint(Variable[] scope,
			MatrixManager matrix, boolean tupleCache) {
		super(scope, tupleCache);
		this.matrix = matrix;
		matrix.setTuple(tuple);
	}

	public AbstractExtensionConstraint(Variable[] scope,
			MatrixManager matrix, String name, boolean tupleCache) {
		super(scope, tupleCache, name);
		this.matrix = matrix;
		matrix.setTuple(tuple);
	}

	@Override
	public MatrixManager getMatrix() {
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
	
	public String getType() {
		return super.getType() + " w/ " + matrix.getType();
	}
}
