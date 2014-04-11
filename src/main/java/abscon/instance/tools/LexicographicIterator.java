package abscon.instance.tools;

/**
 * This class allows iterating all tuples in a lexicograpic order from a given set of domains.
 */
public class LexicographicIterator {
	private int[] tuple;

	private int[] position;

	private int[][] domains;

	public LexicographicIterator(int[][] domains) {
		this.domains = domains;
		tuple = new int[domains.length];
		position = new int[domains.length];
	}

	public int[] getFirstTuple() {
		for (int i = 0; i < tuple.length; i++) {
			tuple[i] = domains[i][0];
			position[i] = 0;
		}
		return tuple;
	}

	public int[] getNextTupleAfter(int[] tuple) {
		for (int i = tuple.length - 1; i >= 0; i--) {
			if (position[i] < domains[i].length - 1) {
				position[i]++;
				tuple[i] = domains[i][position[i]];
				return tuple;
			}
			tuple[i] = domains[i][0];
			position[i] = 0;
		}
		return null;
	}
}
