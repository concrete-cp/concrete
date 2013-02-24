package rb.randomlists;

import java.util.Arrays;

/**
 * This class allows generating integer random lists using a proportion model.
 * This way of generating random lists do not suffer from space complexity considerations.
 */
public class CoarseProportionRandomListGenerator extends ProportionRandomListGenerator
{
	private static final int RANDOMS_LIMIT = 4;
	private static final int OVERFLOWS_LIMIT = 35;

	/**
	 * Builds a coarse proportion random list generator.
	 * @param nbValues the number of values for each element of the tuples
	 * @param seed the seed used to generate random numbers
	 */
	public CoarseProportionRandomListGenerator(int[] nbValues, long seed)
	{
		super(nbValues, seed);
	}

   /**
    * Builds a coarse proportion random list generator.
    * @param nb the uniform number of values used to build tuples
    * @param tupleLength the length of each tuple
    * @param seed the seed used to generate random numbers
    */
   public CoarseProportionRandomListGenerator(int nb, int tupleLength, long seed)
	{
		super(nb, tupleLength, seed);
	}

	private boolean isValidValue(int[] element, int position)
	{
		int value = element[position];
		if (!valueRepetition)
			for (int i = 0; i < position; i++)
				if (element[i] == value)
					return false;
		if (mustValueWait(value))
		{
			if (nbOccurences[value] <= nbMaxOccurences)
				nbCurrentOverflows--;
			nbOccurences[value]--;
			assert(nbOccurences[value] <= nbMaxOccurences && nbCurrentOverflows <= nbAllowedOverflows);
			return false;
		}
		return true;
	}

	private boolean isAuthorizedElement(int[] element, int limit)
	{
		if (fixedTuple != null && !requiredFixedTuple && Arrays.equals(element, fixedTuple))
			return false;

		if (tupleRepetition)
			return true;
		for (int i = 0; i < limit; i++)
			if (Arrays.equals(tuples[i], element) == true)
				return false;
		return true;
	}

	private void manageRequiredElement()
	{
		if (fixedTuple != null && requiredFixedTuple)
		{
			if (mustTupleWait(fixedTuple)) // to keep absolutely in order to update counters
				throw new AssertionError();
			System.arraycopy(fixedTuple, 0, tuples[0], 0, fixedTuple.length);
		}
	}

	private void doPotentialRelaxation(int nbTrials)
	{
		if (nbTrials % OCCURENCES_LIMIT == 0)
			nbMaxOccurences++;
		else if (nbTrials % OVERFLOWS_LIMIT == 0)
			nbAllowedOverflows++;
	}

	protected void makeSelection()
	{
		manageRequiredElement();
		for (int i = (fixedTuple != null && requiredFixedTuple ? 1 : 0); i < tuples.length; i++)
		{
			storeNbOccurrences();
			int nbTrials = 0;
			do
			{
				boolean valid = false;
				while (!valid)
				{
					restoreNbOccurrences();
					valid = true;
					for (int j = 0; valid && j < tuples[i].length; j++)
					{
						int nbRandomUses = 0;
						do tuples[i][j] = random.nextInt(nbValues[j]);
						while (!isValidValue(tuples[i], j) && nbRandomUses++ < (RANDOMS_LIMIT * nbValues[j]));
						if (nbRandomUses >= RANDOMS_LIMIT * nbValues[j])
							valid = false;
					}
					if (!valid)
						doPotentialRelaxation(++nbTrials);
					else if (!valueRepetition)
						Arrays.sort(tuples[i]);
				}
				doPotentialRelaxation(++nbTrials);
			}
			while (!isAuthorizedElement(tuples[i], i));
		}
	}
}
