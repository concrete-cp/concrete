package cspfj.constraint;

import java.util.Arrays;

import cspfj.exception.MatrixTooBigException;
import cspfj.problem.Variable;
import cspfj.util.BooleanArray;

public final class MatrixManager {
	private int[][][] matrix2D = null;

	private boolean[] generalMatrix = null;

	private final int[] domainSize;

	private boolean active = false;

	private final int arity;

	private final int[] tuple;

	private final Variable[] variables;

	private int variablePosition;

	private int[] mask;

	private int[] domain;

	private int currentPart;

	private int currentPosition;

	private int current;

	public MatrixManager(Variable[] scope, int[] tuple) {
		super();

		variables = scope;
		domainSize = new int[scope.length];
		arity = scope.length;
		for (int i = scope.length; --i >= 0;) {
			domainSize[i] = scope[i].getDomain().length;
		}
		this.tuple = tuple;

	}

	private int matrixIndex(final int[] tuple) {
		final int[] domainSize = this.domainSize;
		int index = 0;
		for (int i = this.domainSize.length; --i >= 0;) {
			int skip = 1;
			for (int j = i; --j >= 0;) {
				skip *= domainSize[j];
			}
			index += skip * tuple[i];
		}
		return index;
	}

	public void init(final boolean initialState) throws MatrixTooBigException {
		// assert !active;
		if (arity == 2) {

			matrix2D = new int[2][][];

			for (int i = 2; --i >= 0;) {
				matrix2D[i] = new int[domainSize[i]][BooleanArray
						.booleanArraySize(domainSize[1 - i])];
				for (int[] array : matrix2D[i]) {
					BooleanArray.initBooleanArray(array, domainSize[1 - i],
							initialState);
				}
			}

		} else {

			long nbValues = 1;
			for (int size : domainSize) {
				nbValues *= size;
				if (nbValues > Integer.MAX_VALUE) {
					throw new MatrixTooBigException();
				}
			}

			try {
				generalMatrix = new boolean[(int) nbValues];
			} catch (OutOfMemoryError e) {
				throw new MatrixTooBigException();
			}

			Arrays.fill(generalMatrix, initialState);
		}

		active = true;

	}

	public boolean removeTuple(final int[] tuple) {

		if (!active) {
			try {
				init(true);
			} catch (MatrixTooBigException e) {
				return false;
			}

		}

		if (set(tuple, false)) {
			return true;
		}
		return false;
	}

	public boolean set(final int[] tuple, final boolean status) {
		if (arity == 2) {
			BooleanArray.set(matrix2D[0][tuple[0]], tuple[1], status);
			return BooleanArray.set(matrix2D[1][tuple[1]], tuple[0], status);

		}
		final boolean[] matrix = this.generalMatrix;
		final int matrixIndex = matrixIndex(tuple);

		if (matrix[matrixIndex] == status) {
			return false;
		}
		matrix[matrixIndex] ^= true;
		return true;

	}

	public void clear() {
		matrix2D = null;
		generalMatrix = null;
		active = false;
	}

	public boolean isTrue(final int[] tuple) {
		if (!active) {
			return true;
		}
		if (tuple.length == 2) {
			return BooleanArray.isTrue(matrix2D[0][tuple[0]], tuple[1]);
		}

		return generalMatrix[matrixIndex(tuple)];

	}

	public void intersect(final Variable[] scope,
			final Variable[] constraintScope, final boolean supports,
			final int[][] tuples) throws MatrixTooBigException {

		final boolean[] generalMatrix;

		final int[][][] matrix2D;

		final int arity = domainSize.length;

		if (!active) {
			generalMatrix = null;
			matrix2D = null;
		} else if (arity == 2) {
			matrix2D = this.matrix2D.clone();
			generalMatrix = null;
		} else {
			generalMatrix = this.generalMatrix.clone();
			matrix2D = null;
		}

		init(!supports);

		final int[] realTuple = this.tuple;
		if (scope == constraintScope) {
			for (int[] tuple : tuples) {
				for (int i = arity; --i >= 0;) {
					realTuple[i] = scope[i].index(tuple[i]) ;
				}
				set(realTuple, supports);
			}
		} else {
			
			for (int[] tuple : tuples) {
				// final Variable[] involvedVariables = this.involvedVariables;

				for (int i = arity; --i >= 0;) {
					for (int j = arity; --j >= 0;) {
						if (scope[i] == constraintScope[j]) {
							realTuple[j] = scope[i].index(tuple[i]);
							break;
						}
					}
				}
				set(realTuple, supports);
			}
		}

		if (arity == 2 && matrix2D != null) {
			for (int i = 2; --i >= 0;) {
				for (int j = matrix2D[1 - i].length; --j >= 0;) {
					for (int k = matrix2D[1 - i][j].length; --k >= 0;) {
						this.matrix2D[1 - i][j][k] &= matrix2D[1 - i][j][k];
					}
				}
			}
		} else if (generalMatrix != null) {
			for (int i = generalMatrix.length; --i >= 0;) {
				this.generalMatrix[i] &= generalMatrix[i];
			}
		}
	}

	public boolean isActive() {
		return active;
	}

	public boolean setFirstTuple(final int variablePosition, final int index) {
		this.variablePosition = variablePosition;

		if (isActive() && arity == 2) {
			tuple[variablePosition] = index;

			mask = matrix2D[variablePosition][index];
			domain = variables[1 - variablePosition].getBooleanDomain();
			currentPart = -1;
			current = 0;

			return next();

		}
		for (int position = arity; --position >= 0;) {
			if (position == variablePosition) {
				tuple[position] = index;
			} else {
				tuple[position] = variables[position].getFirstPresentIndex();
			}
		}

		if (!isTrue(tuple)) {
			return next();
		}

		return true;

	}

	public boolean next() {
		if (isActive() && arity == 2) {
			int currentPosition = this.currentPosition ;
			int current = this.current;
			int currentPart = this.currentPart;

			int[] mask = this.mask;
			int[] domain = this.domain;

			while (current == 0) {
				currentPart++;

				if (currentPart >= mask.length) {
					return false;
				}

				current = mask[currentPart] & domain[currentPart];
				// System.err.println(Integer.toBinaryString(current));
				currentPosition = -1;
			}

			
			currentPosition ++ ;
			// System.err.print(Integer.toBinaryString(current)) ;

			int mask0 = BooleanArray.MASKS[0];

			while ((current & mask0) == 0) {
				currentPosition++;
				current <<= 1;
			}

			current <<= 1;

			this.currentPart = currentPart;
			this.current = current;
			this.currentPosition = currentPosition;

			tuple[1 - variablePosition] = currentPart * Integer.SIZE
					+ currentPosition;
			// System.err.println(" - " + tuple[1 - variablePosition]) ;
			//System.err.println(Integer.toBinaryString(current) + " " + currentPart + " " + currentPosition);
			return true;
		}

		if (!setNextTuple()) {
			return false;
		}

		while (!isTrue(tuple)) {
			if (!next()) {
				return false;
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
}
