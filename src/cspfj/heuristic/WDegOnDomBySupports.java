package cspfj.heuristic;

import cspfj.problem.Problem;
import cspfj.problem.Variable;

public class WDegOnDomBySupports implements Heuristic {

    private final Variable[] variables;

    public WDegOnDom wdegOnDom;

    public JW jw;

    public WDegOnDomBySupports(Problem problem) {
        variables = problem.getVariables();
        wdegOnDom = new WDegOnDom(problem);
        jw = new JW(problem);
    }

    public Pair selectPair() {
        Variable bestVariable = null;

        for (Variable v : variables) {
            if (!v.isAssigned()
                    && (bestVariable == null || (v.getDomainSize() != 1 && compare(
                            v, bestVariable) > 0))) {
                bestVariable = v;
            }
        }

        return new Pair(bestVariable, jw.selectIndex(bestVariable));
    }

    public int compare(final Variable variable1, final Variable variable2) {
        final int int1 = jw.selectIndex(variable1);
        final int int2 = jw.selectIndex(variable2);

        final double result = wdegOnDom.getScore(variable1)
                * jw.getScore(variable1, int1) - wdegOnDom.getScore(variable2)
                * jw.getScore(variable2, int2);
        
//        final double result = wdegOnDom.getScore(variable1)
//        - wdegOnDom.getScore(variable2)
//        ;

        if (result == 0) {
            return variable1.getId() - variable2.getId();
        }

        return result > 0 ? 1 : -1;
    }
}
