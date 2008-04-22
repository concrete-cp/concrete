/*
 * Created on 7 nov. 2006
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package cspfj.heuristic;

import java.util.Arrays;
import java.util.Comparator;
import java.util.logging.Logger;

import cspfj.problem.Problem;
import cspfj.problem.Variable;

public abstract class AbstractStaticValueHeuristic extends
		AbstractValueHeuristic {

	private final int[][] order;

	private final static Logger logger = Logger
			.getLogger("cspfj.heuristic.AbstractStaticValueHeuristic");

	private final boolean failFirst;

	public AbstractStaticValueHeuristic(Problem problem, boolean failFirst) {
		super(problem);
		order = new int[problem.getMaxVId() + 1][];
		this.failFirst = failFirst;
	}

	public int[] getOrder(final Variable variable) {
		return order[variable.getId()];

	}

	public final void compute() {
		logger.info("Reordering values with " + this.getClass());
		myCompute();

		final Integer[] orig = new Integer[problem.getMaxDomainSize()];
		for (int i = orig.length; --i >= 0;) {
			orig[i] = i;
		}

		for (Variable variable : problem.getVariables()) {

			final Integer[] ord = new Integer[variable.getDomain().length];
			System.arraycopy(orig, 0, ord, 0, ord.length);

			Arrays.sort(ord, new Sorter(variable));

			variable.reOrder(ord);

		}

	}

	public abstract void myCompute();

	@Override
	public int selectIndex(final Variable variable) {
		return failFirst ? variable.getLast() : variable.getFirst();
	}

	public final boolean isFailFirst() {
		return failFirst;
	}

	private final class Sorter implements Comparator<Integer> {

		private final Variable variable;

		public Sorter(Variable variable) {
			this.variable = variable;
		}

		@Override
		public int compare(final Integer arg0, final Integer arg1) {
			return Double.compare(getScore(variable, arg0), getScore(variable,
					arg1));
		}

	}
}
