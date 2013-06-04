package concrete.heuristic;

import concrete.Variable;
import concrete.Problem

trait ValueHeuristic {
    def selectIndex(variable: Variable): Int

    def score(variable: Variable, index: Int): Double;

    def compute(problem: Problem);
}
