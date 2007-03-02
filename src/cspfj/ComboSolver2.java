package cspfj;

import java.io.IOException;
import java.util.logging.Logger;

import cspfj.filter.AC3_P;
import cspfj.filter.Filter;
import cspfj.filter.SAC;
import cspfj.heuristic.Heuristic;
import cspfj.problem.Problem;

public class ComboSolver2 extends AbstractSolver {

    final RunMCSolver mCSolver;

    final private static Logger logger = Logger.getLogger("cspfj.ComboSolver2");

    public ComboSolver2(Problem prob, ResultHandler resultHandler,
            Heuristic heuristic, boolean reverse) {
        super(prob, resultHandler);
        mCSolver = new RunMCSolver(prob, resultHandler, reverse,
                chronometer);

    }

    public String getXMLConfig() {
        final StringBuffer sb = new StringBuffer(150);

        // sb.append("\t\t\t<macSolver>\n").append(macSolver.getXMLConfig())
        // .append("\t\t\t</macSolver>\n") ;

        sb.append("\t\t\t<mcSolver>\n").append(mCSolver.getXMLConfig()).append(
                "\t\t\t</mcSolver>\n\t\t\t<space>").append(useSpace()).append(
                "</space>\n");

        return sb.toString();
    }

    public synchronized boolean runSolver() throws IOException {
        System.gc();

        final Filter preprocessor;
        switch (useSpace()) {
        case BRANCH:
            preprocessor = new SAC(problem, new AC3_P(problem), true);
            break;

        case CLASSIC:
            preprocessor = new SAC(problem, new AC3_P(problem), false);
            break;

        default:
            preprocessor = new AC3_P(problem);
        }

        if (!preprocessor.reduceAll(0)) {
            chronometer.validateChrono();
            return false;
        }

        mCSolver.start();

        final boolean result = mCSolver.getResult();
        this.setSolution(mCSolver.getSolution());
        return result ;
    }

}
