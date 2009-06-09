package cspfj;

import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.security.InvalidParameterException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.Set;
import java.util.logging.Logger;

import cspfj.constraint.Constraint;
import cspfj.exception.MaxBacktracksExceededException;
import cspfj.filter.AC3;
import cspfj.problem.LSConstraint;
import cspfj.problem.LSVariable;
import cspfj.problem.Problem;
import cspfj.problem.Variable;
import cspfj.util.TieManager;

public abstract class AbstractLocalSolver extends AbstractSolver implements
        LocalSolver {

    final public static Random RANDOM = new Random(0);

    final private static Logger logger = Logger
            .getLogger("cspfj.AbstractLocalSolver");

    // protected static final boolean FINER = logger.isLoggable(Level.FINER);

    private final TieManager tieManager;

    final private boolean max;

    // final protected Map<Variable, LSVariable> lsVariables;
    final protected List<LSVariable> lsVariablesList;
    final protected Map<Constraint, LSConstraint> lsConstraints;

    private int maxTries = -1;

    public static void setSeed(final long seed) {
        RANDOM.setSeed(seed);
    }

    public void setMaxTries(final int tries) {
        this.maxTries = tries;
    }

    public AbstractLocalSolver(final Problem prob,
            final ResultHandler resultHandler, final boolean max) {
        super(prob, resultHandler);

        this.max = max;

        tieManager = new TieManager(RANDOM);

        final Map<Variable, LSVariable> lsVariables = new HashMap<Variable, LSVariable>(
                prob.getNbVariables());
        lsVariablesList = new ArrayList<LSVariable>(prob.getNbVariables());
        for (Variable v : prob.getVariables()) {
            final LSVariable lsv = new LSVariable(v, tieManager);
            lsVariables.put(v, lsv);
            lsVariablesList.add(lsv);
        }

        lsConstraints = new HashMap<Constraint, LSConstraint>(prob
                .getNbConstraints());
        for (Constraint c : prob.getConstraints()) {
            lsConstraints.put(c, new LSConstraint(c, lsVariables));
        }

        for (LSVariable v : lsVariablesList) {
            v.setLSConstraints(lsConstraints);
        }

        setMaxBacktracks(150000);
    }

//    protected int realConflicts() {
//        int realConflicts = 0;
//
//        for (LSConstraint c : lsConstraints.values()) {
//            if (!c.check()) {
//                realConflicts++;
//            }
//        }
//
//        return realConflicts;
//    }

    // protected double weightedConflicts() {
    // double weightedConflicts = 0;
    //
    // for (Constraint c : problem.getConstraints()) {
    // if (!c.checkFirst()) {
    // weightedConflicts += weights[c.getId()];
    // }
    // }
    //
    // return weightedConflicts;
    // }

    // public void increaseWeight(final Constraint c) {
    // weights[c.getId()]++;
    // }
    //
    // public double getWeight(final Constraint c) {
    // return weights[c.getId()];
    // }

    protected int conflicts() {
        int conflicts = 0;
        for (LSConstraint c : lsConstraints.values()) {
            if (!c.check()) {
                conflicts++;
            }
        }
        return conflicts;
    }

    protected void solution(final int nbConflicts) throws IOException {
        final Map<Variable, Integer> solution = new HashMap<Variable, Integer>();
        for (LSVariable v : lsVariablesList) {
            solution.put(v.getVariable(), v.getVariable().getDomain().value(
                    v.getAssignedIndex()));
        }
        setSolution(solution);
        solution(solution, nbConflicts);

    }

    private void init(final LSVariable variable) {
        if (variable.getAssignedIndex() >= 0) {
            return;
        }

        variable.assignBestInitialIndex();

        // if (FINER) {
        logger.finer(variable.toString() + " ("
                + variable.getVariable().getDomainSize() + ") <- "
                + variable.getAssignedIndex());
        // }

        final Set<LSVariable> neighbours = new HashSet<LSVariable>();

        for (LSConstraint c : variable.getLSConstraints()) {
            for (LSVariable v : c.getScope()) {
                if (v != variable) {
                    neighbours.add(v);
                }
            }
        }

        final List<LSVariable> shuffle = new ArrayList<LSVariable>(neighbours);
        Collections.shuffle(shuffle, RANDOM);

        for (LSVariable wcm : shuffle) {
            init(wcm);
        }
    }

    protected void init() {
        final List<LSVariable> shuffle = new ArrayList<LSVariable>(
                lsVariablesList);
        Collections.shuffle(shuffle, RANDOM);

        for (LSVariable v : shuffle) {
            init(v);
        }

        for (LSVariable v : shuffle) {
            v.initNbConflicts();
        }

    }

    public abstract void minConflicts() throws MaxBacktracksExceededException,
            IOException;

    public boolean runSolver() throws IOException {
        final int localBT = getMaxBacktracks();
        boolean resolved = false;
        System.gc();
        chronometer.startChrono();

        try {
            if (!max && !preprocess(new AC3(problem))) {
                return false;
            }
        } catch (InstantiationException e1) {
            throw new InvalidParameterException(e1.toString());
        } catch (IllegalAccessException e1) {
            throw new InvalidParameterException(e1.toString());
        } catch (InvocationTargetException e1) {
            throw new InvalidParameterException(e1.toString());
        } catch (NoSuchMethodException e1) {
            throw new InvalidParameterException(e1.toString());
        } catch (InterruptedException e) {
            // No problem...
        }
        final float start = chronometer.getCurrentChrono();

        int nbTries = 0;
        do {
            if (nbTries++ >= maxTries && maxTries > 0) {
                return false;
            }
            setMaxBacktracks(localBT);
            logger.info("Run with " + localBT + " flips");
            final int nbAssign = getNbAssignments();
            float localTime = -chronometer.getCurrentChrono();
            try {
                minConflicts();
                resolved = true;
            } catch (MaxBacktracksExceededException e) {
                //
            } catch (OutOfMemoryError e) {
                chronometer.validateChrono();
                throw e;
            } catch (IOException e) {
                chronometer.validateChrono();
                throw e;
            }
            localTime += chronometer.getCurrentChrono();

            logger.info("Took " + localTime + " s (" + (localBT / localTime)
                    + " flips per second), " + (getNbAssignments() - nbAssign)
                    + " assignments made");
            // for (Constraint c : problem.getConstraints()) {
            // c.setWeight(1);// Math.max(1, c.getWeight()
            // // / problem.getNbConstraints()));
            // }

            for (LSVariable v : lsVariablesList) {
                v.assign(-1);
            }

        } while (!resolved);
        final float searchCpu = chronometer.getCurrentChrono() - start;
        statistics.put("search-cpu", searchCpu);

        return true;

    }

    public void collectStatistics() {
        chronometer.validateChrono();
    }

    public String getXMLConfig() {
        return new StringBuffer(200).append("\t\t\t<solver>").append(this)
                .append("</solver>\n\t\t\t<maxCSP>").append(max).append(
                        "</maxCSP>\n\t\t\t<maxIterations>").append(
                        getMaxBacktracks()).append("</maxIterations>\n")
                .toString();
    }

    protected TieManager getTieManager() {
        return tieManager;
    }

}