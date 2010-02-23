package cspfj.generator.extension;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import cspfj.constraint.extension.Matrix;
import cspfj.constraint.extension.TupleSet;
import cspom.extension.Extension;
import cspom.variable.Domain;

public class ExtensionToTupleList extends AbstractExtensionConverter {
	public ExtensionToTupleList(final DomainConverter domains) {
		super(domains);
	}

	// private final static Logger logger = Logger
	// .getLogger(ExtensionToTupleList.class.getSimpleName());

	@Override
	public Matrix convert(final Extension relation,
			final cspom.variable.CSPOMVariable[] signature) {
		if (!(relation instanceof cspom.extension.Extension)) {
			return null;
		}
		final cspom.extension.Extension extension = (cspom.extension.Extension) relation;

		final Domain[] domains = cspomDomains(signature);

		// logger.fine(relation + " to TupleList");

		BigInteger size = BigInteger.ONE;
		for (int s : sizes(domains)) {
			size = size.multiply(BigInteger.valueOf(s));
		}

		final Matrix matrix;

		// if (size.compareTo(BigInteger.valueOf(Integer.MAX_VALUE)) >= 0
		// || (extension.getNbTuples() < size.intValue() / 5 && extension
		// .isSupports())) {
		// matrix = new TupleArray();
		// } else {
		matrix = new TupleSet(extension.getNbTuples(), !extension.isSupports());
		// }

		final int[] tuple = new int[signature.length];
		final List<Map<Number, Integer>> indexes = new ArrayList<Map<Number, Integer>>(
				domains.length);
		for (Domain d : domains) {
			indexes.add(this.domains.getIndexMap(d));
		}
		for (Number[] values : extension.getTuples()) {
			for (int i = tuple.length; --i >= 0;) {
				tuple[i] = indexes.get(i).get(values[i]);
			}
			matrix.set(tuple, extension.isSupports());
		}

		return matrix;

	}
}
