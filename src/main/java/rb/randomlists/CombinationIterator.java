package rb.randomlists;

import java.util.Arrays;

/**
 * This class allows iterating all combinations of a given length from a given
 * propagationSet of numbers.
 */
public final class CombinationIterator {
	/**
	 * The number of values used for each element of the different combinations.
	 * The array index corresponds to the order of the elements of the
	 * combinations. The array value gives the number of values for the
	 * corresponding element. For instance, if nbValues[i] = n then it means
	 * that the ith element of each combination can belong to the propagationSet
	 * <code> {0,1,...,n-1} </code>.
	 */
	private final int[] nbValues;

	/**
	 * The length of each combination
	 */
	private final int length;

	/**
	 * Indicates if the numbers of values used for each element of the different
	 * combinations are all equal.
	 */
	private final boolean uniform;

	/**
	 * The current combination
	 */
	private int[] currentElement;

	/**
	 * Indicates if the method <code> hasNext </code> has already been called.
	 * Indeed, in order to know if there is another combination, one has to
	 * compute it. So we need to be careful not to consecutively call
	 * <code> hasNext </code> two times.
	 */
	private boolean computedNextValue;

	/**
	 * Indicates the return value of the method <code> hasNext </code> in case
	 * of it has already been called.
	 */
	private boolean hasNextValue;

	/**
	 * Builds an object to iterate combinations.
	 * 
	 * @param nbValues
	 *            the number of values for each element of the combinations.
	 *            These numbers must be in an increasing order.
	 */
	public CombinationIterator(int[] nbValues) {
		this.nbValues = (int[]) nbValues.clone();
		this.length = nbValues.length;
		boolean uniform = true;
		for (int i = 1; i < nbValues.length; i++) {
			if (nbValues[i - 1] > nbValues[i]) {
				throw new IllegalArgumentException(
						"The propagationSet of numbers of values are not in an increasing order");
			}
			if (nbValues[i] != nbValues[0]) {
				uniform = false;
			}
		}
		this.uniform = uniform;
		currentElement = new int[length];
		reset();
	}

	/**
	 * Builds an object to iterate combinations.
	 * 
	 * @param nb
	 *            the uniform number of values used to form combinations
	 * @param length
	 *            the length of each combination
	 */
	public CombinationIterator(int nb, int length) {
		if (nb < 1 || length < 1 || nb < length) {
			throw new IllegalArgumentException();
		}
		nbValues = new int[length];
		Arrays.fill(nbValues, nb);
		this.length = length;
		this.uniform = true;
		currentElement = new int[length];
		reset();
	}

	/**
	 * Resets the iterator.
	 */
	public void reset() {
		computedNextValue = true;
		hasNextValue = true;
		for (int i = 0; hasNextValue && i < length; i++) {

			if (i >= nbValues[i]) {
				hasNextValue = false;
			} else {
				currentElement[i] = i;
			}
		}
	}

	/**
	 * Determines if it is possible to extend the current element from the given
	 * index
	 * 
	 * @param from
	 * @return
	 */
	private boolean isExtensibleFrom(final int index) {
		if (uniform) {
			return (nbValues[index] - currentElement[index] > length - index);
		}

		int value = currentElement[index];
		for (int i = index; i < currentElement.length; i++) {
			if (++value >= nbValues[i]) {
				return false;
			}
		}
		return true;
	}

	/**
	 * Determines if there is another combination (greater than the current
	 * one).
	 */
	public boolean hasNext() {
		if (computedNextValue) {
			return hasNextValue;
		}
		computedNextValue = true;

		int last = length - 1;
		while (last >= 0) {
			if (!isExtensibleFrom(last)) {
				last--;
			} else {
				currentElement[last] = currentElement[last] + 1;
				for (int i = last + 1; i < length; i++)
					currentElement[i] = currentElement[i - 1] + 1;
				hasNextValue = true;
				return true;
			}
		}
		hasNextValue = false;
		return false;
	}

	/**
	 * Returns the next combination.
	 */
	public int[] next() {
		if (!computedNextValue)
			hasNext();
		assert hasNextValue;
		computedNextValue = false;
		return currentElement;
	}

	/**
	 * Displays all different combinations.
	 */
	public String allCombinations() {
		final StringBuffer buf = new StringBuffer();
		reset();
		int cpt = 0;
		while (hasNext()) {
			final int[] element = next();
			buf.append('(');
			for (int i = 0; i < element.length; i++) {
				if (i != 0) {
					buf.append(',');
				}
				buf.append(element[i]);
			}
			buf.append(")  ");
			cpt++;
		}
		buf.append("\nThere are ").append(cpt).append(" combinations");
		return buf.toString();
	}

	public static void main(final String[] args) {
		final int[] nbValues = { 7, 7, 7, 7 };
		// int arity = Integer.parseInt(args[1]);

		final CombinationIterator iterator = new CombinationIterator(nbValues); // ,
		// arity);
		System.out.println(RandomListGenerator
				.computeNbCombinationsFrom(nbValues));
		System.out.println(iterator.allCombinations());
	}
}
