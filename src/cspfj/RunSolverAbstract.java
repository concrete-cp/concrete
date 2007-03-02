package cspfj;

import java.io.IOException;
import java.util.Map;
import java.util.logging.Logger;

import cspfj.problem.Problem;
import cspfj.problem.Variable;
import cspfj.util.Chronometer;

public abstract class RunSolverAbstract extends Thread {

    final private Problem problem;

    private Solver solver;

    final private static Logger logger = Logger
            .getLogger("cspfj.RunSolverAbstract");

    final private Chronometer chronometer;

    private boolean solved = false;

    private boolean result;

    protected RunSolverAbstract(final Problem problem,
            final ResultHandler resultHandler, final boolean reverse,
            final Chronometer chronometer) {
        this.problem = problem;
        this.chronometer = chronometer;

    }

    protected void setSolver(Solver solver){
        this.solver = solver ;
    }
    
    public synchronized void run() {
        try {
            float maxTries = 1;

            do {
                logger.info("MC with " + (int) Math.floor(maxTries) + " x "
                        + solver.getMaxBacktracks() + " flips");
                float localTime = -chronometer.getCurrentChrono();
                int assign = -solver.getNbAssignments();
                
                    if (launch((int)Math.floor(maxTries))) {
                        solved = true;
                        result = true;
                        notifyAll() ;
                        return;
                    }

                    for (Variable v : problem.getVariables()) {
                        v.resetAssign();
                    }

                    problem.restoreAll(1);
                
                localTime += chronometer.getCurrentChrono();
                assign += solver.getNbAssignments();
                logger
                        .info("Took "
                                + localTime
                                + " s ("
                                + (solver.getMaxBacktracks() * (int) Math.floor(maxTries) / localTime)
                                + " flips per second), " + assign
                                + " assignments made");

                maxTries *= 1.5;

            } while (true);
        } catch (IOException e) {

        }

    }

    protected abstract boolean launch(final int factor) throws IOException;

    public synchronized boolean getResult() {
        while (!solved) {
            try {
                wait();
            } catch (InterruptedException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
        }
        return result;
    }

    public synchronized Map<Variable, Integer> getSolution() {
        return solver.getSolution();
    }

    public String getXMLConfig() {
        return solver.getXMLConfig();
    }

    protected Problem getProblem() {
        return problem;
    }
}
