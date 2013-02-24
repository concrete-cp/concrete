package rb.randomlists;

import java.util.Comparator;

/**
 * A comparator that allows to put arrays of integers in a lexicographic order.
 */
public class LexicographicComparator implements Comparator<int[]> {
	public int compare(int[] t1, int[] t2) {
		for (int i = 0; i < t1.length; i++) {
			if (t1[i] < t2[i])
				return -1;
			if (t1[i] > t2[i])
				return +1;
		}
		return 0;
	}
}
