package rb.randomlists;

import java.util.Arrays;

/**
 * This class allows generating integer random lists using a proportion model.
 * Tuples are selected by managing arrays containing indexes coresponding to
 * tuples. Indeed, using the number of values as a base, it is possible to
 * associate an index with each tuple. This way of generating random lists can
 * only be performed with a limited number of potential tuples.
 */
public class FineProportionRandomListGenerator extends
		ProportionRandomListGenerator {
	private static final int STRUCTURES_LIMIT = 100000;

	private long[] validIndexes;
	private int nbValidIndexes;

	private long[] waitingIndexes;
	private int nbWaitingIndexes;

	/**
	 * Builds a fine proportion random list generator.
	 * 
	 * @param nbValues
	 *            the number of values for each element of the tuples
	 * @param seed
	 *            the seed used to generate random numbers
	 */
	public FineProportionRandomListGenerator(int[] nbValues, long seed) {
		super(nbValues, seed);
	}

	/**
	 * Builds a fine proportion random list generator.
	 * 
	 * @param nb
	 *            the uniform number of values used to build tuples
	 * @param tupleLength
	 *            the length of each tuple
	 * @param seed
	 *            the seed used to generate random numbers
	 */
	public FineProportionRandomListGenerator(int nb, int tupleLength, long seed) {
		super(nb, tupleLength, seed);
	}

	public void setParameters(Structure type, boolean tupleRepetition,
			boolean valueRepetition, int[] fixedTuple,
			boolean requiredFixedTuple) {
		super.setParameters(type, tupleRepetition, valueRepetition, fixedTuple,
				requiredFixedTuple);
		if (tupleRepetition)
			throw new IllegalArgumentException(
					"It is not possible to use a fine proportion random list generator with tuple repetition");
		if (computeNbDistinctTuples() > STRUCTURES_LIMIT)
			throw new IllegalArgumentException(
					"It is not possible to use a fine proportion random list generator with so many potential tuples");
	}

	private void fillIndexStructures() {
		int nbDistinctTuples = (int) Math.ceil(computeNbDistinctTuples());
		nbWaitingIndexes = 0;
		waitingIndexes = new long[nbDistinctTuples];
		nbValidIndexes = nbDistinctTuples;
		validIndexes = new long[nbDistinctTuples];

		CombinationIterator iterator = (valueRepetition ? null
				: new CombinationIterator(nbValues));
		for (int i = 0; i < validIndexes.length; i++)
			validIndexes[i] = (valueRepetition ? i : BasisConvertor
					.getDecimalValueFor(iterator.next(), nbValues));
	}

	private void manageRequiredElement() {
		if (fixedTuple != null) {
			int position = Arrays.binarySearch(validIndexes, BasisConvertor
					.getDecimalValueFor(fixedTuple, nbValues));
			validIndexes[position] = validIndexes[--nbValidIndexes];
			if (!requiredFixedTuple)
				return;
			if (mustTupleWait(fixedTuple)) // to keep absolutely in order to
											// update counters
				throw new AssertionError();
			System.arraycopy(fixedTuple, 0, tuples[0], 0, fixedTuple.length);
		}
	}

	protected void makeSelection() {
		fillIndexStructures();
		manageRequiredElement();

		for (int i = (fixedTuple != null && requiredFixedTuple ? 1 : 0); i < tuples.length; i++) {
			storeNbOccurrences();
			boolean found = false;
			int nbTrials = 0;
			while (!found) {
				nbTrials++;
				while (!found && nbValidIndexes > 0) {
					restoreNbOccurrences();
					int rand = random.nextInt(nbValidIndexes);
					long index = validIndexes[rand];
					tuples[i] = BasisConvertor.getValueFor(index, tuples[i],
							nbValues);
					if (mustTupleWait(tuples[i]))
						waitingIndexes[nbWaitingIndexes++] = index;
					else
						found = true;
					validIndexes[rand] = validIndexes[--nbValidIndexes];
				}
				if (!found) {
					if (nbTrials % OCCURENCES_LIMIT == 0)
						nbMaxOccurences++;
					else
						nbAllowedOverflows++;
					nbValidIndexes = nbWaitingIndexes;
					for (int j = 0; j < nbValidIndexes; j++)
						validIndexes[j] = waitingIndexes[j];
					nbWaitingIndexes = 0;
				}
			}
		}
	}
}
