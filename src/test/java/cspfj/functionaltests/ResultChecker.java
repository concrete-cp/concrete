package cspfj.functionaltests;

import static org.junit.Assert.assertEquals;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;

import javax.script.ScriptException;

import cspfj.ResultHandler;
import cspfj.problem.Variable;

public class ResultChecker extends ResultHandler {
    private Throwable throwable;

    public cspom.CSPOM checker;

    @Override
    public void result(Result result, Throwable thrown) throws IOException {
        super.result(result, thrown);
        this.throwable = thrown;
    }

    public void setChecker(cspom.CSPOM checker) {
        this.checker = checker;
    }

    public boolean solution(final Map<Variable, Integer> solution,
            final int nbConflicts, final boolean force) throws IOException {
        final List<Number> flatSolution = new ArrayList<Number>(solution.size());
        for (Variable v : getSolver().getProblem().getVariables()) {
            flatSolution.add(solution.get(v));
        }

        final Collection<cspom.constraint.CSPOMConstraint> falsified;
        try {
            falsified = checker.checkSolution(flatSolution);
        } catch (ScriptException e) {
            // Should not happen at this point
            throw new IllegalArgumentException(e);
        }
        assertEquals(nbConflicts, falsified.size());

        return super.solution(solution, nbConflicts, force);

    }

    public Throwable getThrowable() {
        return throwable;
    }
}