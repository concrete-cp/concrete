package rb.randomlists;

import java.util.Collection;
import java.util.LinkedList;


/**
 * This class allows generating integer random lists using a probability model.
 * All potential tuples are selected according a probability given by the user. 
 * With this model, generated lists are UNSTRUCTURED and tuples can not occur several times.
 */
public class ProbabilityRandomListGenerator extends RandomListGenerator
{
	/**
	 * The limit (a real value ranging from 0 to 1) used for selection. 
	 */
	private double selectionLimit;

	/**
	 * Builds a probability random list generator.
	 * @param nbValues the number of values for each element of the tuples
	 * @param seed the seed used to generate random numbers
	 */
	public ProbabilityRandomListGenerator(int[] nbValues, long seed)
	{
		super(nbValues, seed);
		type = Structure.UNSTRUCTURED;
		tupleRepetition = false;
	}

	/**
	 * Builds a probability random list generator.
	 * @param nb the uniform number of values used to build tuples
	 * @param tupleLength the length of each tuple
	 * @param seed the seed used to generate random numbers
	 */
	public ProbabilityRandomListGenerator(int nb, int tupleLength, long seed)
	{
		super(nb, tupleLength, seed);
		type = Structure.UNSTRUCTURED;
		tupleRepetition = false;
	}

	/**
	 * Returns the selection limit while taking into account the management of a fixed tuple.
	 */
	private double getActualSelectionLimit(double nbMaxElements)
	{
		if (fixedTuple == null)
			return selectionLimit;
		if (requiredFixedTuple)
			return Math.max(0, (selectionLimit - 1.0 / nbMaxElements)) * (nbMaxElements / (nbMaxElements - 1));
		return (selectionLimit * nbMaxElements) / (nbMaxElements - 1);

	}

	/**
	 * Generates and returns a random list.
	 * @param selectionLimit the limit (a real value ranging from 0 to 1) used for selection  
	 * @param valueRepetition indicates if the same value can occur several times in a generated tuple
	 * @param fixedTuple a particular tuple, which if not <code> null </code>, must or must not belong to the generated lists 
	 * @param requiredFixedTuple indicates if the fixed tuple, if not <code> null </code>, must or must not belong to the generated lists 
	 * @return a random generated list
	 */
	public int[][] selectTuples(double selectionLimit, boolean valueRepetition, int[] fixedTuple, boolean requiredFixedTuple)
	{
		setParameters(Structure.UNSTRUCTURED, false, valueRepetition, fixedTuple, requiredFixedTuple);
		this.selectionLimit = selectionLimit;

		Collection<int[]> linkedList = new LinkedList<int[]>();
		double nbDistinctTuples = computeNbDistinctTuples();
		double actualSelectionLimit = getActualSelectionLimit(nbDistinctTuples);
		long fixedIndex = (fixedTuple == null ? -1 : BasisConvertor.getDecimalValueFor(fixedTuple, nbValues));
		CombinationIterator iterator = (valueRepetition ? null : new CombinationIterator(nbValues));
		int[] element = new int[tupleLength];
		for (int i = 0; i < nbDistinctTuples; i++)
		{
			element = (valueRepetition ? BasisConvertor.getValueFor(i, element, nbValues) : iterator.next());
			if (BasisConvertor.getDecimalValueFor(element, nbValues) == fixedIndex)
			{
				if (!requiredFixedTuple)
					continue;
			}
			else if (random.nextDouble() > actualSelectionLimit)
				continue;
			updateNbValueOccurencesFor(element);
			linkedList.add(element.clone());
		}

		tuples = (int[][]) linkedList.toArray(new int[0][]);
		return tuples;
	}

	/**
	 * Generates and returns a random list.
	 * @param selectionLimit the limit (a real value ranging from 0 to 1) used for selection  
	 * @param valueRepetition indicates if the same value can occur several times in a generated tuple
	 * @return a random generated list
	 */
	public int[][] selectTuples(double selectionLimit, boolean valueRepetition)
	{
		return selectTuples(selectionLimit, valueRepetition, null, false);
	}

	protected String getSelectionDescription()
	{
		return tuples.length + "(" + selectionLimit + ")";
	}

	static void main(String[] args)
	{
		int nbValues = Integer.parseInt(args[0]);
		int arity = Integer.parseInt(args[1]);
		int seed = Integer.parseInt(args[2]);
		double selectionLimit = Double.parseDouble(args[3]);
		boolean vrepetition = (Integer.parseInt(args[4]) != 0);
		ProbabilityRandomListGenerator r = new ProbabilityRandomListGenerator(nbValues, arity, seed);
		r.selectTuples(selectionLimit, vrepetition);
		r.displayTuples();
		//r.saveElements();
	}
}
