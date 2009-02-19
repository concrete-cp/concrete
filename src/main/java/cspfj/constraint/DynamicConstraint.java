package cspfj.constraint;


public interface DynamicConstraint extends Constraint {
	int removeTuples(int[] tuple);
	boolean removeTuple(int[] tuple);
	boolean positive();
}
