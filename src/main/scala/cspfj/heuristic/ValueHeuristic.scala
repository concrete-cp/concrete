package cspfj.heuristic;

import cspfj.problem.Variable;

trait ValueHeuristic {
    def selectIndex(variable: Variable): Int

    def score(variable: Variable, index: Int): Double;

    def compute();
}
