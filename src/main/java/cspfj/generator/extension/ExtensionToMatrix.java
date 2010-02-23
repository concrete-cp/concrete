package cspfj.generator.extension;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import cspfj.constraint.extension.Matrix;
import cspfj.constraint.extension.Matrix2D;
import cspfj.constraint.extension.MatrixGeneral;
import cspfj.constraint.extension.TupleSet;
import cspom.extension.Extension;
import cspom.variable.Domain;

public class ExtensionToMatrix extends AbstractExtensionConverter {
	public ExtensionToMatrix(final DomainConverter domains) {
		super(domains);
	}

	// private final static Logger logger = Logger
	// .getLogger(ExtensionToMatrix.class.getSimpleName());

	@Override
	public Matrix convert(final Extension relation,
			final cspom.variable.CSPOMVariable[] signature) {
		if (!(relation instanceof cspom.extension.Extension)) {
			return null;
		}
		final cspom.extension.Extension extension = (cspom.extension.Extension) relation;

		final Domain[] domains = cspomDomains(signature);
		final int[] sizes = sizes(domains);

		final Matrix matrix;

		if (extension.getArity() == 2) {
			// logger.fine(relation + " to Matrix2D");
			matrix = new Matrix2D(sizes[0], sizes[1], !extension.isSupports());

		} else {
			if (extension.isSupports()
					&& TupleSet.isBetterThanMatrix(sizes, extension
							.getNbTuples())) {
				return null;
			}

			matrix = new MatrixGeneral(sizes, !extension.isSupports());
		}

		final int[] tuple = new int[sizes.length];
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
