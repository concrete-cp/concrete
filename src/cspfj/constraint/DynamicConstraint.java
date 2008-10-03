package cspfj.constraint;


public interface DynamicConstraint extends Constraint {
	int removeTuple(int[] tuple);
	int removeTuples(int[] fixed, int mobilePosition, int[] values);
}
