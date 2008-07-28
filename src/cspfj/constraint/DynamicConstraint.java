package cspfj.constraint;

public interface DynamicConstraint extends Constraint {
	public boolean removeTuple(int[] realTuple);
}
