package cspfj.util;

import cspfj.problem.IntIterator;

public interface BitVector extends Cloneable {

	void fill(final int size, final boolean fill);

	boolean set(final int position, final boolean status);

	boolean clear(final int position);

	boolean set(final int position);

	boolean get(final int position);

	int nextSetBit(final int start);

	int prevSetBit(final int start);

	int prevClearBit(final int start);

	void setSingle(final int index);

	void clearFrom(final int from);

	IntIterator iterator();

	void copyTo(BitVector bV);

	boolean intersects(BitVector bV, final int position);

	int intersects(BitVector bV);

	BitVector clone();

}