package cspfj.constraint;

public interface DynamicConstraint extends Constraint {
	boolean removeTuple(int[] realTuple);
}
