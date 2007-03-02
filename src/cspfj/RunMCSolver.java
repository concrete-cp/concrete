package cspfj;

import java.io.IOException;
import java.util.logging.Logger;

import cspfj.constraint.Constraint;
import cspfj.exception.MaxBacktracksExceededException;
import cspfj.problem.Problem;
import cspfj.util.Chronometer;

public class RunMCSolver extends AbstractRunSolver {

    private final MinConflictsSolver mCSolver;

    final private static Logger logger = Logger
            .getLogger("cspfj.RunMinConflictsSolver");

    public RunMCSolver(final Problem problem,
            final ResultHandler resultHandler, final boolean reverse,
            final Chronometer chronometer) {
        super(problem, chronometer);
        mCSolver = new MinConflictsSolver(problem, resultHandler, reverse) ;
        setSolver(mCSolver);
        mCSolver.setMaxBacktracks(problem.getMaxFlips());
        
    }

    @Override
    protected boolean launch(int factor) throws IOException {
		logger.info("MC with " + factor +" x "+mCSolver.getMaxBacktracks()+ " flips");
        for (int i = factor; --i >= 0;) {

            try {

                mCSolver.minConflicts();

                return true;
            } catch (MaxBacktracksExceededException e) {

            }

            for (Constraint c : getProblem().getConstraints()) {
                c.setWeight(c.getWeight() / getProblem().getNbConstraints());
            }
            logger.info("Max constraint weight : "
                    + getProblem().getMaxWeight());

            return false;
        }

        // TODO Auto-generated method stub
        return false;
    }

}
