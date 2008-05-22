package cspfj.constraint;

import java.util.Comparator;

import cspfj.heuristic.WeightHeuristic;

public final class Weight implements Comparator<AbstractConstraint> {

	final private boolean reverse;

	final private WeightHeuristic wvh;

	public Weight(final boolean reverse, final WeightHeuristic wvh) {
		super();
		this.reverse = reverse;
		this.wvh = wvh;
	}

	public int compare(final AbstractConstraint arg0, final AbstractConstraint arg1) {
		final int compare = Double.compare(wvh.getWeight(arg0), wvh
				.getWeight(arg1));

		if (compare == 0) {
			return arg0.getId() - arg1.getId();
		}

		return reverse ? -compare : compare;
	}

}
