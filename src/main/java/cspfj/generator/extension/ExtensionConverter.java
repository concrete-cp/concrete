package cspfj.generator.extension;

import cspfj.constraint.extension.Matrix;
import cspom.extension.Extension;

public interface ExtensionConverter {

	Matrix convert(final Extension relation,
			final cspom.variable.CSPOMVariable[] signature);


}
