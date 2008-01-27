package cspfj.constraint;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class TupleList2 implements Matrix, Cloneable {

	private List<int[]> list;

	private final boolean supports;

	public TupleList2(int[] sizes, boolean supports) {
		this.supports = supports ^ true;
		list = new ArrayList<int[]>();

	}

	public boolean check(final int[] tuple) {
		for (int [] t: list) {
			if (Arrays.equals(tuple, t)) {
				return supports;
			}
		}
		return supports ^true;
	}

	@Override
	public void set(final int[] tuple, final boolean status) {
		if (status == supports) {
			list.add(tuple);
		} else {
			list.remove(tuple);
		}
	}

	public TupleList2 clone() throws CloneNotSupportedException {
		final TupleList2 list = (TupleList2) super.clone();

		list.list = new ArrayList<int[]>(this.list);

		return list;
	}

}
