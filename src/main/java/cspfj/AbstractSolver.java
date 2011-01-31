/**
 * CSPFJ - CSP solving API for Java
 * Copyright (C) 2006 Julien VION
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
 */

package cspfj;

import java.lang.reflect.InvocationTargetException;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Timer;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import cspfj.constraint.AbstractAC3Constraint;
import cspfj.constraint.AbstractConstraint;
import cspfj.constraint.extension.MatrixManager2D;
import cspfj.exception.MaxBacktracksExceededException;
import cspfj.filter.Filter;
import cspfj.problem.Problem;
import cspfj.problem.Variable;
import cspfj.util.MsLogHandler;
import cspfj.util.Parameter;
import cspfj.util.Statistic;
import cspfj.util.Waker;
import cspom.CSPOM;

public abstract class AbstractSolver implements Solver {
    private static final Logger LOGGER = Logger.getLogger(AbstractSolver.class
            .getName());
    public static final String VERSION;

    @Parameter("logger.level")
    private static String loggerLevel = "WARNING";
    @Parameter("solver")
    private static Class<? extends Solver> solverClass = MGACIter.class;

    static {
        Matcher matcher = Pattern.compile("Rev:\\ (\\d+)").matcher(
                "$Rev$");
        matcher.find();
        VERSION = matcher.group(1);
        ParameterManager.register(AbstractSolver.class);

    }

    public static Solver factory(Problem problem) {
        final Solver solver;
        try {
            solver = solverClass.getConstructor(Problem.class).newInstance(
                    problem);
        } catch (Exception e) {
            throw new IllegalArgumentException(e);
        }
        StatisticsManager.register("solver", solver);
        return solver;
    }

    public static Solver factory(CSPOM problem) {
        final Solver solver;
        try {
            solver = solverClass.getConstructor(CSPOM.class).newInstance(
                    problem);
        } catch (Exception e) {
            throw new IllegalArgumentException(e);
        }
        StatisticsManager.register("solver", solver);
        return solver;
    }

    protected final Problem problem;

    private int maxBacktracks;

    @Parameter("preprocessor")
    private static Class<? extends Filter> preprocessorClass = null;

    private int preproExpiration = -1;

    private int nbBacktracks;

    public AbstractSolver(final Problem prob) {
        super();

        problem = prob;

        final Level level = Level.parse(loggerLevel);

        Logger.getLogger("").setLevel(level);
        for (Handler h : Logger.getLogger("").getHandlers()) {
            Logger.getLogger("").removeHandler(h);
        }

        final Handler handler = new MsLogHandler(System.currentTimeMillis());
        handler.setLevel(level);
        Logger.getLogger("").addHandler(handler);

        LOGGER.info(ParameterManager.list());
    }

    public final void setMaxBacktracks(final int maxBacktracks) {
        this.maxBacktracks = maxBacktracks;
        this.nbBacktracks = 0;
    }

    public final void checkBacktracks() throws MaxBacktracksExceededException {
        if (++nbBacktracks >= maxBacktracks && maxBacktracks >= 0) {
            throw new MaxBacktracksExceededException();
        }
    }

    public final int getMaxBacktracks() {
        return maxBacktracks;
    }

    protected final int getNbBacktracks() {
        return nbBacktracks;
    }

    public final void setPreproExp(final int time) {
        this.preproExpiration = time;
    }

    protected final Map<String, Integer> solution() {
        final Map<String, Integer> solution = new LinkedHashMap<String, Integer>();
        for (Variable v : problem.getVariables()) {
            solution.put(v.getName(), v.getValue(v.getFirst()));
        }
        return solution;
    }

    public final Problem getProblem() {
        return problem;
    }

    public final boolean preprocess(final Filter filter)
            throws InterruptedException {

        LOGGER.info("Preprocessing (" + preproExpiration + ")");

        final Filter preprocessor;
        if (preprocessorClass == null) {
            preprocessor = filter;
        } else {
            try {
                preprocessor = preprocessorClass.getConstructor(Problem.class)
                        .newInstance(problem);
            } catch (InstantiationException e) {
                throw new IllegalArgumentException(e);
            } catch (IllegalAccessException e) {
                throw new IllegalArgumentException(e);
            } catch (InvocationTargetException e) {
                throw new IllegalArgumentException(e);
            } catch (NoSuchMethodException e) {
                throw new IllegalArgumentException(e);
            }
            StatisticsManager.register("preprocessor", preprocessor);
        }

        Thread.interrupted();

        final Timer waker = new Timer();

        if (preproExpiration >= 0) {
            waker.schedule(new Waker(Thread.currentThread()),
                    preproExpiration * 1000);
        }

        long preproCpu = -System.currentTimeMillis();
        boolean consistent;
        try {
            consistent = preprocessor.reduceAll();
        } catch (InterruptedException e) {
            LOGGER.warning("Interrupted preprocessing");
            consistent = true;
            throw e;
        } catch (OutOfMemoryError e) {
            LOGGER.throwing("Filter", "reduceAll", e);
            throw e;
        } finally {
            preproCpu += System.currentTimeMillis();
            waker.cancel();

            preproRemoved = 0;

            for (Variable v : problem.getVariables()) {
                preproRemoved += v.getDomain().maxSize() - v.getDomainSize();
            }
            this.preproCpu = preproCpu / 1000f;
            preproConstraintChecks = AbstractAC3Constraint.getChecks();
            preproPresenceChecks = AbstractConstraint.getPresenceChecks();
            preproMatrix2DChecks = MatrixManager2D.getChecks();
            preproMatrix2DPresenceChecks = MatrixManager2D.getPresenceChecks();
        }

        return consistent;

    }

    @Statistic
    public int preproRemoved;
    @Statistic
    public double preproCpu;
    @Statistic
    public long preproConstraintChecks;
    @Statistic
    public long preproPresenceChecks;
    @Statistic
    public long preproMatrix2DChecks;
    @Statistic
    public long preproMatrix2DPresenceChecks;

    public String getXMLConfig() {
        return ParameterManager.toXML();
    }

}
