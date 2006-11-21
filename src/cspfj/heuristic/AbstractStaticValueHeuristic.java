/*
 * Created on 7 nov. 2006
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package cspfj.heuristic;

import java.util.logging.Logger;

import cspfj.problem.Problem;
import cspfj.problem.Variable;

public abstract class AbstractStaticValueHeuristic extends
		AbstractValueHeuristic {

	private final int[][] order;

	private final static Logger logger = Logger
			.getLogger("cspfj.heuristic.AbstractStaticValueHeuristic");

	public AbstractStaticValueHeuristic(Problem problem) {
		super(problem);
		order = new int[problem.getMaxVId() + 1][];
	}

	public int[] getOrder(final Variable variable) {
		return order[variable.getId()];

	}

	public void compute() {
		logger.info("Reordering values with " + this.getClass());
		for (Variable variable : variables) {

			final int[] ord = new int[variable.getDomain().length];
			// order[variable.getId()] = ord;

			for (int i = ord.length; --i >= 0;) {
				ord[i] = i;
			}

			// for (int i = ord.length; --i >= 0;) {
			// for (int j = i; --j >= 0;) {
			// // System.out.println(i+":"+getScore(v, ord[i]) + " " +
			// // j+":"+getScore(v, ord[j])) ;
			// if (getScore(variable, ord[i]) < getScore(variable, ord[j])) {
			// swtch(ord, i, j);
			// }
			// }
			// }

			sort(variable, ord, 0, ord.length - 1);

			variable.reOrderHeuristic(ord);
//
//			 System.out.print(variable + " : ");
//			 for (int i = ord.length; --i >= 0;) {
//			 System.out.print(ord[i] + "(" + getScore(variable, ord[i])
//			 + ") ");
//			 }
//			 System.out.println();

		}
	}

	private static void swap(final int[] table, final int i, final int j) {
		final int temp = table[i];
		table[i] = table[j];
		table[j] = temp;
	}

	@Override
	public int selectIndex(final Variable variable) {
		return variable.getFirstHeuristic();
		// final int[] ord = order[variable.getId()];
		// for (int i = ord.length; --i >= 0;) {
		// if (variable.isPresent(ord[i])) {
		// return ord[i];
		// }
		// }
		// return -1;
	}

	private final void sort(final Variable variable, final int[] ord,
			final int left, final int right) {
		if (right > left) {
			//final int pivotIndex = left;
			final int newIndex = partition(variable, ord, left, right);
			sort(variable, ord, left, newIndex - 1);
			sort(variable, ord, newIndex + 1, right);
		}
	}

	private final int partition(final Variable variable, final int[] ord,
			final int left, final int right) {
		final double pivotScore = getScore(variable, ord[left]);
		swap(ord, left, right);
		int storeIndex = left;
		for (int i = left; i < right;i++) {
			if (getScore(variable, ord[i]) <= pivotScore) {
				swap(ord, storeIndex, i);
				storeIndex++;
			}
		}
		swap(ord, right, storeIndex);
		return storeIndex;
	}

}
