package cspfj.constraint.extension;

import cspfj.constraint.DynamicConstraint;

public interface ExtensionConstraint extends DynamicConstraint {
    MatrixManager getMatrixManager();
}
