package cspfj.generator;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import cspfj.constraint.extension.Matrix;
import cspfj.constraint.extension.Matrix2D;
import cspfj.constraint.extension.MatrixGeneral;
import cspfj.constraint.extension.TupleSet;
import cspfj.problem.Domain;
import cspom.extension.Extension;

public class ExtensionGenerator {
	private final static Map<Signature, Matrix> GENERATED = new HashMap<Signature, Matrix>();

	public static Matrix generate(Domain[] domains, Extension<Number> extension) {
		final Signature signature = new Signature(domains, extension);
		Matrix matrix = GENERATED.get(signature);
		if (matrix == null) {
			matrix = bestMatrix(extension, domains);
			fillMatrix(domains, extension, matrix);
			GENERATED.put(signature, matrix);
		}
		return matrix;
	}
	
	public static void clear() {
		GENERATED.clear();
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
			return 31 * Arrays.deepHashCode(domains) + extension.hashCode();
		}

		@Override
		public boolean equals(Object obj) {
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
				|| (4 * nbTuples < size.intValue());
	}

	private static int[] sizes(final Domain[] domains) {
		final int[] sizes = new int[domains.length];
		for (int i = sizes.length; --i >= 0;) {
			sizes[i] = domains[i].size();
		}
		return sizes;
	}

	private static Map<Number, Integer> indexMap(Domain domain) {
		final Map<Number, Integer> map = new HashMap<Number, Integer>(domain
				.size());
		for (int i = domain.first(); i != -1; i = domain.next(i)) {
			map.put(domain.value(i), i);
		}
		return map;
	}

	public static Matrix bestMatrix(final Extension<Number> extension,
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
