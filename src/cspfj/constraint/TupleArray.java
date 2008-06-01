package cspfj.constraint;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class TupleArray implements Matrix, Cloneable {
	// private int last;

	private List<int[]> tuples;

	public TupleArray() {
		tuples = new ArrayList<int[]>();
	}

	@Override
	public boolean check(final int[] tuple) {
		for (int[] t : tuples) {
			if (Arrays.equals(t, tuple)) {
				return true;
			}
		}
		return false;
	}

	@Override
	public void set(final int[] tuple, final boolean status) {
		if (status) {
			tuples.add(tuple.clone());
		} else {
			for (int i = tuples.size(); --i >= 0;) {
				if (Arrays.equals(tuples.get(i), tuple)) {
					tuples.set(i, null);
				}
			}
		}
	}

	public int[] getTuple(final int index) {
		return tuples.get(index);
	}
	
	public int getNbTuples() {
		return tuples.size();
	}

	public TupleArray clone() throws CloneNotSupportedException {
		final TupleArray list = (TupleArray) super.clone();

		list.tuples = new ArrayList<int[]>(tuples);

		return list;
	}

}
