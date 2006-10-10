package cspfj.constraint;

import cspfj.problem.Variable;
import cspfj.util.BooleanArray;

public class TupleIterator {

	// private final Constraint constraint;

	private final int arity;

	private final int[] tuple;

	private final Variable[] variables;

	private int variablePosition;

	private final MatrixManager matrix;

	private int[] mask;

	private int[] domain;

	private int currentPart;

	private int currentPosition;

	private int current;

	public TupleIterator(final Constraint constraint, final int[] tuple) {
		super();
		this.variables = constraint.getInvolvedVariables();
		this.arity = constraint.getArity();
		this.tuple = tuple;
		this.matrix = constraint.getMatrix();
	}

	public boolean setFirstTuple(final int variablePosition, final int index) {
		this.variablePosition = variablePosition;

		if (matrix.isActive() && arity == 20) {
			tuple[variablePosition] = index;

			mask = matrix.get2D(variablePosition, index);
			domain = variables[1 - variablePosition].getBooleanDomain();
			currentPart = -1;
			current = 0;

			return  next();

		} else {
			for (int position = arity; --position >= 0;) {
				if (position == variablePosition) {
					tuple[position] = index;
				} else {
					tuple[position] = variables[position]
							.getFirstPresentIndex();
				}
			}

			if (!matrix.isTrue(tuple)) {
				return  next();
			}
			
			return true ;
		}

	}

	public boolean next() {
		final MatrixManager matrix = this.matrix;

		if (matrix.isActive() && arity == 20) {

			while (current == 0) {
				currentPart++;

				if (currentPart >= mask.length) {
					return false;
				}

				current = mask[currentPart] & domain[currentPart];
				//System.err.println(Integer.toBinaryString(current));
				currentPosition = -1;
			}

			// System.err.print(Integer.toBinaryString(current)) ;
			currentPosition++;

			while ((current & BooleanArray.MASKS[0]) == 0) {
				currentPosition++;
				current <<= 1;
			}

			current <<= 1;

			tuple[1 - variablePosition] = currentPart * Integer.SIZE
					+ currentPosition;
			// System.err.println(" - " + tuple[1 - variablePosition]) ;

		} else {

			if (!setNextTuple()) {
				return false;
			}

			while (!matrix.isTrue(tuple)) {
				if (!next()) {
					return false;
				}
			}

		}

		return true;

	}

	private boolean setNextTuple() {
		final int[] tuple = this.tuple;

		final Variable[] involvedVariables = this.variables;
		for (int i = arity; --i >= 0;) {
			if (i == variablePosition) {
				continue;
			}

			final int index = involvedVariables[i].getNext(tuple[i]);

			if (index < 0) {
				tuple[i] = involvedVariables[i].getFirstPresentIndex();
			} else {
				tuple[i] = index;
				return true;
			}
		}
		return false;

	}

	public void remove() {
		// TODO Auto-generated method stub

	}

//	public static void main(String[] args) {
//		int[] current = new int[] { 0x00000000 };
//
//		System.out.println(BooleanArray.toString(current));
//
//		int currentPosition = -1;
//
//		for (int currentPart = 0; currentPart < current.length; currentPart++) {
//
//			while (current[currentPart] != 0) {
//				currentPosition++;
//				while ((current[currentPart] & BooleanArray.MASKS[0]) == 0) {
//					currentPosition++;
//					current[currentPart] <<= 1;
//				}
//
//				current[currentPart] <<= 1;
//				System.out.print(currentPosition + " ");
//
//			}
//		}
//	}
}
