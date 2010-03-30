package cspfj.heuristic;

import cspfj.problem.Variable;

public final class Lexico implements ValueHeuristic {

    private final boolean reverse;

    public Lexico(final boolean reverse) {
        this.reverse = reverse;
    }

    public double getScore(final Variable var, final int index) {
        if (reverse) {
            return -index;
        }
        return index;
    }

    public String toString() {
        if (reverse) {
            return "min-lexico";
        }
        return "max-lexico";
    }

    @Override
    public void compute() {
        // Nothing to compute
    }

    @Override
    public int selectIndex(final Variable variable) {
        if (reverse) {
            return variable.getLast();
        }
        return variable.getFirst();
    }
}
