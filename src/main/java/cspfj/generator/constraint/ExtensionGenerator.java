package cspfj.generator.constraint;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import cspfj.constraint.Constraint;
import cspfj.constraint.extension.ExtensionConstraint2D;
import cspfj.constraint.extension.ExtensionConstraintDynamic;
import cspfj.constraint.extension.ExtensionConstraintGeneral;
import cspfj.constraint.extension.Matrix;
import cspfj.constraint.extension.Matrix2D;
import cspfj.constraint.extension.MatrixGeneral;
import cspfj.constraint.extension.TupleSet;
import cspfj.exception.FailedGenerationException;
import cspfj.problem.Domain;
import cspfj.problem.Problem;
import cspfj.problem.Variable;
import cspom.constraint.CSPOMConstraint;
import cspom.extension.Extension;

public final class ExtensionGenerator extends AbstractGenerator {
	private static final int TIGHTNESS_LIMIT = 4;
	private static final int HASHCODE_BASE = 31;
	private final Map<Signature, Matrix> generated = new HashMap<Signature, Matrix>();

	public ExtensionGenerator(final Problem problem) {
		super(problem);
	}

	private Matrix generate(final Domain[] domains,
			final Extension<Number> extension) {
		final Signature signature = new Signature(domains, extension);
		Matrix matrix = generated.get(signature);
		if (matrix == null) {
			matrix = bestMatrix(extension, domains);
			fillMatrix(domains, extension, matrix);
			generated.put(signature, matrix);
		}
		return matrix;
	}

	@Override
	public boolean generate(final CSPOMConstraint constraint)
			throws FailedGenerationException {

		final Variable[] solverVariables = getSolverVariables(constraint
				.getScope());

		final Domain[] domains = new Domain[constraint.getArity()];
		for (int i = constraint.getArity(); --i >= 0;) {
			final Domain domain = solverVariables[i].getDomain();
			if (domain == null) {
				return false;
			}
			domains[i] = domain;
		}

		final cspom.extension.ExtensionConstraint<Number> extConstraint = (cspom.extension.ExtensionConstraint<Number>) constraint;
		final Matrix matrix = generate(domains, extConstraint.getRelation());

		final Constraint gConstraint;
		if (matrix instanceof Matrix2D) {
			gConstraint = new ExtensionConstraint2D(solverVariables,
					(Matrix2D) matrix, true);
		} else if (matrix instanceof TupleSet) {
			gConstraint = new ExtensionConstraintDynamic(solverVariables,
					(TupleSet) matrix, true);
		} else {
			gConstraint = new ExtensionConstraintGeneral(matrix, true,
					solverVariables);
		}
		addConstraint(gConstraint);
		return true;

	}

	private static class Signature {
		private Domain[] domains;
		private Extension<Number> extension;

		public Signature(final Domain[] domains,
				final Extension<Number> extension) {
			this.domains = domains;
			this.extension = extension;
		}

		@Override
		public int hashCode() {
			return HASHCODE_BASE * Arrays.deepHashCode(domains)
					+ extension.hashCode();
		}

		@Override
		public boolean equals(final Object obj) {
			if (!(obj instanceof Signature)) {
				return false;
			}
			final Signature sign = (Signature) obj;
			return extension == sign.extension
					&& Arrays.equals(domains, sign.domains);
		}
	}

	private static boolean tupleSetBetterThanMatrix(final Domain[] domains,
			final int nbTuples) {
		BigInteger size = BigInteger.ONE;
		for (Domain d : domains) {
			size = size.multiply(BigInteger.valueOf(d.size()));
		}

		return size.compareTo(BigInteger.valueOf(Integer.MAX_VALUE)) >= 0
				|| (TIGHTNESS_LIMIT * nbTuples < size.intValue());
	}

	private static int[] sizes(final Domain[] domains) {
		final int[] sizes = new int[domains.length];
		for (int i = sizes.length; --i >= 0;) {
			sizes[i] = domains[i].size();
		}
		return sizes;
	}

	private static Map<Number, Integer> indexMap(final Domain domain) {
		final Map<Number, Integer> map = new HashMap<Number, Integer>(domain
				.size());
		for (int i = domain.first(); i != -1; i = domain.next(i)) {
			map.put(domain.value(i), i);
		}
		return map;
	}

	private static Matrix bestMatrix(final Extension<Number> extension,
			final Domain[] domains) {
		if (!extension.init()
				&& tupleSetBetterThanMatrix(domains, extension.getNbTuples())) {
			return new TupleSet(extension.getNbTuples(), extension.init());
		}
		if (extension.getArity() == 2) {
			// logger.fine(relation + " to Matrix2D");
			return new Matrix2D(domains[0].size(), domains[1].size(), extension
					.init());
		}
		return new MatrixGeneral(sizes(domains), extension.init());

	}

	private static void fillMatrix(final Domain[] domains,
			final Extension<Number> extension, final Matrix matrix) {
		final int[] tuple = new int[domains.length];
		final List<Map<Number, Integer>> indexes = new ArrayList<Map<Number, Integer>>(
				domains.length);
		for (Domain d : domains) {
			indexes.add(indexMap(d));
		}
		for (Number[] values : extension.getTuples()) {
			for (int i = tuple.length; --i >= 0;) {
				tuple[i] = indexes.get(i).get(values[i]);
			}
			matrix.set(tuple, !extension.init());
		}

	}

}
