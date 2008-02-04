/*
 * Created on 7 nov. 2006
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package cspfj.heuristic;

import java.util.ArrayList;
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

	private final boolean failFirst ;
	
	public AbstractStaticValueHeuristic(Problem problem, boolean failFirst) {
		super(problem);
		order = new int[problem.getMaxVId() + 1][];
		this.failFirst = failFirst ;
	}

	public int[] getOrder(final Variable variable) {
		return order[variable.getId()];

	}

	public final void compute() {
		logger.info("Reordering values with " + this.getClass());
		myCompute() ;
		
		final Integer[] orig = new Integer[problem.getMaxDomainSize()] ;
		for (int i = orig.length; --i >= 0;) {
			orig[i] = i;
		}

		
		for (Variable variable : problem.getVariables()) {

			// order[variable.getId()] = ord;
			final Integer[] ord = new Integer[variable.getDomain().length] ;
			System.arraycopy(orig, 0, ord, 0, ord.length);
			// for (int i = ord.length; --i >= 0;) {
			// for (int j = i; --j >= 0;) {
			// // System.out.println(i+":"+getScore(v, ord[i]) + " " +
			// // j+":"+getScore(v, ord[j])) ;
			// if (getScore(variable, ord[i]) < getScore(variable, ord[j])) {
			// swtch(ord, i, j);
			// }
			// }
			// }

//			sort(variable, ord, 0, variable.getDomain().length - 1);

			Arrays.sort(ord, new Sorter(variable));

			variable.reOrder(ord);
			//
			// System.out.print(variable + " : ");
			// for (int i = ord.length; --i >= 0;) {
			// System.out.print(ord[i] + "(" + getScore(variable, ord[i])
			// + ") ");
			// }
			// System.out.println();
//			final StringBuffer sb = new StringBuffer() ;
//			for (int i : ord) {
//				sb.append(variable.getDomain()[i]+" "+getScore(variable,i)+"\n");
//			}
//			logger.finer(variable.toString()+"\n"+sb);
		}

	}
	
	public abstract void myCompute() ;

	private static void swap(final int[] table, final int i, final int j) {
		final int temp = table[i];
		table[i] = table[j];
		table[j] = temp;
	}

	@Override
	public int selectIndex(final Variable variable) {
		if (failFirst) {
			return variable.getLast() ;
		} else {
			return variable.getFirst();
		}
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
			// final int pivotIndex = left;
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
		for (int i = left; i < right; i++) {
			if (getScore(variable, ord[i]) <= pivotScore) {
				swap(ord, storeIndex, i);
				storeIndex++;
			}
		}
		swap(ord, right, storeIndex);
		return storeIndex;
	}

	public final boolean isFailFirst() {
		return failFirst ;
	}
	
	private final class Sorter implements Comparator<Integer> {

		private final Variable variable;
		
		public Sorter(Variable variable) {
			this.variable = variable;
		}
		
		@Override
		public int compare(Integer arg0, Integer arg1) {
			return Double.compare(getScore(variable, arg0), getScore(variable, arg1));
		}
		
	}
}
