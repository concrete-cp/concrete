package abscon.instance.components;

import java.util.Arrays;

import abscon.instance.InstanceTokens;
import abscon.instance.Toolkit;

public class PRelation {

	protected String name;

	protected int nbTuples;

	protected int arity;

	protected String semantics;

	protected int[][] tuples;

	public String getName() {
		return name;
	}

	public int getArity() {
		return arity;
	}

	public String getSemantics() {
		return semantics;
	}

	public int[][] getTuples() {
		return tuples;
	}


	public int getMaximalCost() {
		return 1;
	}

	public PRelation(String name, int arity, int nbTuples, String semantics, int[][] tuples) {
		this.name = name;
		this.arity = arity;
		this.nbTuples = nbTuples;
		this.semantics = semantics;
		this.tuples = tuples;
		Arrays.sort(tuples,Toolkit.lexicographicComparator); 
	}


	public int computeCostOf(int[] tuple) {
		int position = Arrays.binarySearch(tuples, tuple, Toolkit.lexicographicComparator);
		if (semantics.equals(InstanceTokens.SUPPORTS))
			return position >= 0 ? 0 : 1;
		return position >= 0 ? 1 : 0;
	}

	public String toString() {
		int displayLimit = 5;
		String s = "  relation " + name + " with arity=" + arity + ", semantics=" + semantics + ", nbTuples=" + nbTuples + " : ";
		for (int i = 0; i < Math.min(nbTuples, displayLimit); i++) {
			s += "(";
			for (int j = 0; j < arity; j++)
				s += (tuples[i][j] + (j < arity - 1 ? "," : ""));
			s += ") ";
		}
		return s + (nbTuples > displayLimit ? "..." : "");
	}

	public boolean isSimilarTo(int arity, int nbTuples, String semantics, int[][] tuples) {
		if (this.arity != arity || this.nbTuples != nbTuples)
			return false;
		if (!this.semantics.equals(semantics))
			return false;
		for (int i = 0; i < tuples.length; i++)
			for (int j = 0; j < tuples[i].length; j++)
				if (this.tuples[i][j] != tuples[i][j])
					return false;
		return true;
	}

	public String getStringListOfTuples() {
		StringBuffer s = new StringBuffer();
		for (int i = 0; i < tuples.length; i++) {
			for (int j = 0; j < tuples[i].length; j++) {
				s.append(tuples[i][j]);
				if (j != tuples[i].length - 1)
					s.append(' ');
			}
			if (i != tuples.length - 1)
				s.append('|');
		}
		return s.toString();
	}

}
