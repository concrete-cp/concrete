package abscon.instance.components;

import java.util.Arrays;

import abscon.instance.InstanceTokens;
import abscon.instance.Toolkit;

public class PSoftRelation extends PRelation {

	private int[] weights;

	/**
	 * defaultCost = Integer.MAX_VALUE if defaultCost is infinity
	 */
	private int defaultCost;

	/**
	 * The max of all weights values and defaultCost.
	 */
	private int maximalCost;

	public int[] getWeights() {
		return weights;
	}

	public int getDefaultCost() {
		return defaultCost;
	}

	public int getMaximalCost() {
		return maximalCost;
	}

	public PSoftRelation(String name, int arity, int nbTuples, String semantics, int[][] tuples, int[] weights, int defaultCost) {
		super(name, arity, nbTuples, semantics, tuples);
		this.weights = weights;
		this.defaultCost = defaultCost;
		maximalCost = defaultCost;
		for (int w : weights)
			if (w > maximalCost)
				maximalCost = w;
	}

	public int computeCostOf(int[] tuple) {
		int position = Arrays.binarySearch(tuples, tuple, Toolkit.lexicographicComparator);
		return position >= 0 ? weights[position] : defaultCost;
	}

	public String toString() {
		int displayLimit = 5;
		String s = "  relation " + name + " with arity=" + arity + ", semantics=" + semantics + ", nbTuples=" + nbTuples + ", defaultCost=" + defaultCost + " : ";
		for (int i = 0; i < Math.min(nbTuples, displayLimit); i++) {
			s += "(";
			for (int j = 0; j < arity; j++)
				s += (tuples[i][j] + (j < arity - 1 ? "," : ""));
			s += ") ";
			if (weights != null)
				s += " with cost=" + weights[i] + ", ";
		}
		return s + (nbTuples > displayLimit ? "..." : "");
	}

	public boolean isSimilarTo(int arity, int nbTuples, String semantics, int[][] tuples, int[] weights, int defaultCost) {
		if (!super.isSimilarTo(arity, nbTuples, semantics, tuples))
			return false;
		if (this.defaultCost != defaultCost)
			return false;
		for (int i = 0; i < weights.length; i++)
			if (this.weights[i] != weights[i])
				return false;
		return true;
	}

	public String getStringListOfTuples() {
		StringBuffer sb = new StringBuffer();
		int currentWeigth = -1;
		for (int i = 0; i < tuples.length; i++) {
			if (i != 0)
				sb.append("|");
			if (weights[i] != currentWeigth) {
				currentWeigth = weights[i];
				sb.append(currentWeigth + InstanceTokens.COST_SEPARATOR);
			}
			for (int j = 0; j < tuples[i].length; j++) {
				sb.append(tuples[i][j]);
				if (j != tuples[i].length - 1)
					sb.append(' ');
			}
		}
		return sb.toString();
	}

}
