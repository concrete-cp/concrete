package cspfj.problem;

import cspfj.util.BitVector;

public interface Domain extends Cloneable {
	int first();

	int last();

	int next(int i);

	int prev(int i);

	int lastAbsent();

	int prevAbsent(int i);

	int size();

	int index(int value);

	int value(int index);

	int maxSize();

	boolean present(int index);

	void setSingle(int index);

	void remove(int index);

	/**
	 * @param lb
	 * @return Removes all indexes starting from given lower bound.
	 */
	int removeFrom(int lb);

	/**
	 * @param ub
	 * @return Removes all indexes up to given upper bound.
	 */
	int removeTo(int ub);

	void setLevel(int level);

	void restoreLevel(int level);

	BitVector getAtLevel(int level);

	int[] allValues();

	Domain clone();

	int[] currentValues();

	/**
	 * @param value
	 * @return the index of the closest value lower or equal to the given value.
	 */
	int greatest(int value);

	/**
	 * @param value
	 * @return the index of the closest value greater or equal to the given
	 *         value.
	 */
	int lowest(int value);

}
