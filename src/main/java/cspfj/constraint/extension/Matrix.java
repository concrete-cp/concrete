package cspfj.constraint.extension;

public interface Matrix {

	void set(int[] tuple, boolean status);

	boolean check(int[] tuple);

	Matrix clone();

	boolean isEmpty();

}
