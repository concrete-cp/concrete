package cspfj.constraint.semantic;

import cspfj.constraint.AbstractAC3Constraint;
import cspfj.problem.Variable;

public final class LexLeq extends AbstractAC3Constraint {

    // private final Variable[] vec1;
    // private final Variable[] vec2;
    private final int half;

    public LexLeq(final Variable... scope) {
        super(null, scope);

        if (scope.length % 2 != 0) {
            throw new IllegalArgumentException();
        }
        half = scope.length / 2;
        // vec1 = Arrays.copyOf(scope, half);
        // vec2 = Arrays.copyOfRange(scope, half, scope.length);
    }

    @Override
    public boolean check() {
        for (int i = 0; i < getArity() / 2; i++) {
            final int v0 = getValue(i);
            final int v1 = getValue(i + half);
            if (v0 < v1) {
                return true;
            }
            if (v0 > v1) {
                return false;
            }
        }
        return true;
    }

    // @Override
    // public float getEvaluation() {
    // return half;
    // }
    //
    // @Override
    // public boolean revise(RevisionHandler revisator, int reviseCount) {
    // // TODO Auto-generated method stub
    // return false;
    // }

}
