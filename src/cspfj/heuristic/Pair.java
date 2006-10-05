package cspfj.heuristic;

import cspfj.problem.Variable;

public final class Pair {

    private final Variable variable;

    private final int value;

    public Pair(Variable variable, int value) {
        super();
        this.variable = variable;
        this.value = value;
    }

    public int getValue() {
        return value;
    }

    public Variable getVariable() {
        return variable;
    }

}
