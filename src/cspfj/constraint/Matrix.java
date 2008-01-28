package cspfj.constraint;

public interface Matrix extends Cloneable {

	void set(int[] tuple, boolean status);

	boolean check(int[] tuple);

	Matrix clone() throws CloneNotSupportedException;
}
