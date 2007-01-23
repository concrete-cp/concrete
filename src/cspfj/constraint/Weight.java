package cspfj.constraint;

import java.util.Comparator;

public final class Weight implements Comparator<Constraint> {

	final private boolean reverse;

	public Weight(final boolean reverse) {
		super();
		this.reverse = reverse;
	}

	public int compare(final Constraint arg0, final Constraint arg1) {
		final float compare = arg0.getWeight() - arg1.getWeight();

		final int result ;
		
		if (compare == 0) {
			return arg0.getId() - arg1.getId();
		} else {
			result = compare > 0 ? 1 : -1;
		}
		return reverse ? -result : result;
	}

}
