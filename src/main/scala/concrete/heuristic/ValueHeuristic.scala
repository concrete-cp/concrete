package concrete.heuristic;

import concrete.Variable;

trait ValueHeuristic {
    def selectIndex(variable: Variable): Int

    def score(variable: Variable, index: Int): Double;

    def compute();
}
