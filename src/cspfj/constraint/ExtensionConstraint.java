package cspfj.constraint;


public interface ExtensionConstraint extends Constraint {
	boolean removeTuple();

	AbstractMatrixManager getMatrix();
}
