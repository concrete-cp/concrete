package rb.randomlists;

import java.util.Arrays;

/**
 * This class allows generating integer random lists using a proportion model.
 * A given fixed number of tuples is randomly generated.
 */
public abstract class ProportionRandomListGenerator extends RandomListGenerator
{
	protected static final int OCCURENCES_LIMIT = 100;

	/**
	* The wished number of occurences of each value. 
	*/
	protected int nbWishedOccurences;

	/**
	 * The maximum number of occurences of each value. 
	 */
	protected int nbMaxOccurences;

	/**
	 * The number of allowed overflows wrt the maximum number of occurences of each value. 
	 */
	protected int nbAllowedOverflows;

	/**
	* The current number of overflows wrt the maximum number of occurences of each value. 
	*/
	protected int nbCurrentOverflows;

	private int nbCurrentOverflowsBis;

	private int[] nbOccurencesBis;

	private LexicographicComparator lexicographicComparator = new LexicographicComparator();

	/**
	 * Builds a proportion random list generator.
	 * @param nbValues the number of values for each element of the tuples
	 * @param seed the seed used to generate random numbers
	 */
	public ProportionRandomListGenerator(int[] nbValues, long seed)
	{
		super(nbValues, seed);
		nbOccurencesBis = new int[nbMaxValues];
	}

	/**
	 * Builds a proportion random list generator.
	 * @param nb the uniform number of values used to build tuples
	 * @param tupleLength the length of each tuple
	 * @param seed the seed used to generate random numbers
	 */
	public ProportionRandomListGenerator(int nb, int tupleLength, long seed)
	{
		super(nb, tupleLength, seed);
		nbOccurencesBis = new int[nb];
	}

	/**
	 * Saves the current number of occurences of each value and the current number of overflows.
	 */
	protected void storeNbOccurrences()
	{
		for (int i = 0; i < nbOccurences.length; i++)
			nbOccurencesBis[i] = nbOccurences[i];
		nbCurrentOverflowsBis = nbCurrentOverflows;
	}

	/**
	 * Restores the current number of occurences of each value and the current number of overflows.
	 */
	protected void restoreNbOccurrences()
	{
		for (int i = 0; i < nbOccurences.length; i++)
			nbOccurences[i] = nbOccurencesBis[i];
		nbCurrentOverflows = nbCurrentOverflowsBis;
	}

	/**
	 * Fixes some limits, according to the given type, about the generation of tuples. 
	 * @param type the type of the generated lists which can be UNSTRUCTURED, CONNECTED or BALANCED
	 */
	protected void fixLimits(Structure type)
	{
		int nbTuples = tuples.length;
		switch (type) {
		case UNSTRUCTURED:
			nbWishedOccurences = Integer.MAX_VALUE; //nbTuples;
			nbMaxOccurences = Integer.MAX_VALUE; //nbTuples;
			nbAllowedOverflows = 0;
			break;
		
		case CONNECTED:
			nbWishedOccurences = 1;
			nbMaxOccurences = nbTuples;
			nbAllowedOverflows = tupleLength * nbTuples - (nbWishedOccurences * nbMaxValues);
			break;

		case BALANCED:
			nbWishedOccurences = tupleLength * nbTuples / nbMaxValues;
			nbMaxOccurences = nbWishedOccurences + 1;
			nbAllowedOverflows = tupleLength * nbTuples % nbMaxValues; // O A REMETRE ???
		}
	}

	/**
	 * Determines (according to some selection constraints) if the given value can be currently selected.
	 * @param value a given value
	 * @return <code> false </code> iff the given value can be currently put in a tuple. 
	 */
	protected boolean mustValueWait(int value)
	{
		nbOccurences[value]++;
		if (nbOccurences[value] <= nbWishedOccurences)
			return false;
		if (nbOccurences[value] > nbMaxOccurences)
			return true;
		nbCurrentOverflows++;
		if (nbCurrentOverflows > nbAllowedOverflows)
			return true;
		return false;
	}

	/**
	 * Determines if the given tuple can be currently selected.
	 * @param tuple the given tuple
	 * @return <code> false </code> iff the given value can be currently put in a tuple
	 */
	protected boolean mustTupleWait(int[] tuple)
	{
		for (int i = 0; i < tuple.length; i++)
			if (mustValueWait(tuple[i]))
				return true;
		return false;
	}

	/**
	 * Makes the selection of the given number of tuples.
	 */
	protected abstract void makeSelection();

	/**
	 * Generates and returns a random list.
	 * @param nbTuples the number of tuples to be selected
	 * @param type the type of the generated lists which can be UNSTRUCTURED, CONNECTED or BALANCED
	 * @param tupleRepetition indicates if the same tuple can occur several times in the generated lists
	 * @param valueRepetition indicates if the same value can occur several times in a generated tuple
	 * @param fixedTuple a particular tuple, which if not <code> null </code>, must or must not belong to the generated lists 
	 * @param requiredFixedTuple indicates if the fixed tuple, if not <code> null </code>, must or must not belong to the generated lists 
	 * @return a random generated list
	 */
	public int[][] selectTuples(
		int nbTuples,
		Structure type,
		boolean tupleRepetition,
		boolean valueRepetition,
		int[] fixedTuple,
		boolean requiredFixedTuple)
	{
	//	System.out.println("nbTuples = " + nbTuples + " length = "  );
		setParameters(type, tupleRepetition, valueRepetition, fixedTuple, requiredFixedTuple);
		if (tuples == null || tuples.length != nbTuples)
			tuples = new int[nbTuples][tupleLength];

		if (!tupleRepetition)
		{
			double nbDistinctTuples = computeNbDistinctTuples();
			if (nbTuples > nbDistinctTuples)
				throw new IllegalArgumentException("The number of tuples " + nbTuples + " is greater than the maximum number " + nbDistinctTuples);
		}
		fixLimits(type);
		makeSelection();
		Arrays.sort(tuples, lexicographicComparator);
		return tuples;
	}

	/**
	 * Generates and returns a random list.
	 * @param nbTuples the number of tuples to be selected
	 * @param type the type of the generated lists which can be UNSTRUCTURED, CONNECTED or BALANCED
	 * @param tupleRepetition indicates if the same tuple can occur several times in the generated lists
	 * @param valueRepetition indicates if the same value can occur several times in a generated tuple
	 * @return a random generated list
	 */
	public int[][] selectTuples(int nbTuples, Structure type, boolean tupleRepetition, boolean valueRepetition)
	{
		return selectTuples(nbTuples, type, tupleRepetition, valueRepetition, null, false);
	}

	public void displayTuples()
	{
		super.displayTuples();
		//System.out.println("wished=" + nbWishedOccurences + " max=" + nbMaxOccurences + " over=" + nbAllowedOverflows);
	}

	public static void main(String[] args)
	{
		int nbValues = Integer.parseInt(args[0]);
		int arity = Integer.parseInt(args[1]);
		int seed = Integer.parseInt(args[2]);
		int nbElements = Integer.parseInt(args[3]);
		Structure type = Structure.valueOf(args[4]);
		boolean erepetition = (Integer.parseInt(args[5]) != 0);
		boolean vrepetition = (Integer.parseInt(args[6]) != 0);
		boolean fine = (Integer.parseInt(args[7]) != 0);
		ProportionRandomListGenerator r = null;
		if (fine)
			r = new FineProportionRandomListGenerator(nbValues, arity, seed);
		else
			r = new CoarseProportionRandomListGenerator(nbValues, arity, seed);
		r.selectTuples(nbElements, type, erepetition, vrepetition);
		r.displayTuples();
	}

}
