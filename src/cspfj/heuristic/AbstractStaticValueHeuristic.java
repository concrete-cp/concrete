/*
 * Created on 7 nov. 2006
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package cspfj.heuristic;

import java.util.Arrays;

import cspfj.problem.Problem;
import cspfj.problem.Variable;

public abstract class AbstractStaticValueHeuristic extends
		AbstractValueHeuristic {

	private int[][] order;

	public AbstractStaticValueHeuristic(Problem problem) {
		super(problem);
		order = new int[problem.getMaxVId() + 1][];
	}

	public void compute() {
		for (Variable v : variables) {
			final int[] ord = new int[v.getDomain().length];
			order[v.getId()] = ord;
			
			for (int i = ord.length; --i >= 0;) {
				ord[i] = i;
			}

			for (int i = ord.length; --i >= 0;) {
				for (int j = i ; --j >= 0;) {
					//System.out.println(i+":"+getScore(v, ord[i]) + " " + j+":"+getScore(v, ord[j])) ;
					if (getScore(v, ord[i]) < getScore(v, ord[j])) {
						swtch(ord, i, j);
					}
				}
			}
			//System.out.println(v + " : " + Arrays.toString(ord));
		}
	}

	private static void swtch(final int[] table, final int i, final int j) {
		final int temp = table[i];
		table[i] = table[j];
		table[j] = temp;
	}

	@Override
	public int selectIndex(final Variable variable) {
		final int[] ord = order[variable.getId()];
		for (int i = ord.length; --i >= 0;) {
			if (variable.isPresent(ord[i])) {
				return ord[i];
			}
		}
		return -1;
	}

}
