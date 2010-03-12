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

import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.util.HashMap;
import java.util.Map;
import java.util.Timer;
import java.util.Map.Entry;
import java.util.logging.Logger;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import cspfj.constraint.AbstractConstraint;
import cspfj.constraint.extension.MatrixManager2D;
import cspfj.exception.MaxBacktracksExceededException;
import cspfj.filter.Filter;
import cspfj.problem.Problem;
import cspfj.problem.IntVariable;
import cspfj.util.Chronometer;
import cspfj.util.Waker;

public abstract class AbstractSolver implements Solver {
	public static final String VERSION;
	static {
		Matcher matcher = Pattern.compile("Rev:\\ (\\d+)").matcher(
				"$Rev$");
		matcher.find();
		VERSION = matcher.group(1);
	}
	protected final Problem problem;

	protected final Chronometer chronometer;

	private int nbAssignments;

	private final Map<IntVariable, Integer> solution;

	private int maxBacktracks;

	private int nbBacktracks;

	private int nbSolutions = 0;

	private final ResultHandler resultHandler;

	private Class<? extends Filter> preprocessor = null;

	private int preproExpiration = -1;

	private final static Logger logger = Logger.getLogger(AbstractSolver.class
			.getName());

	protected final Map<String, Object> statistics;

	public static final Map<String, String> parameters = new HashMap<String, String>();

	public AbstractSolver(Problem prob, ResultHandler resultHandler) {
		super();
		problem = prob;
		nbAssignments = 0;
		solution = new HashMap<IntVariable, Integer>();

		chronometer = new Chronometer();
		this.resultHandler = resultHandler;
		this.statistics = new HashMap<String, Object>();
	}

	public int getNbAssignments() {
		return nbAssignments;
	}

	protected final void incrementNbAssignments() {
		nbAssignments++;
	}

	protected final void addSolutionElement(final IntVariable variable,
			final int index) {
		solution.put(variable, variable.getValue(index));
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see cspfj.Solver#getSolution()
	 */
	public final Map<IntVariable, Integer> getSolution() {
		return solution;
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

	public float getUserTime() {
		return chronometer.getUserTime();
	}

	public final int getMaxBacktracks() {
		return maxBacktracks;
	}

	protected final int getNbBacktracks() {
		return nbBacktracks;
	}

	protected final void setSolution(final Map<IntVariable, Integer> solution) {
		this.solution.clear();
		this.solution.putAll(solution);
		nbSolutions = 1;
	}

	protected void solution(final Map<IntVariable, Integer> solution,
			final int nbConflicts) throws IOException {
		if (resultHandler.solution(solution, nbConflicts, false)) {
			logger.info("At " + chronometer.getCurrentChrono());
		}
	}

	public final void setUsePrepro(final Class<? extends Filter> prepro) {
		this.preprocessor = prepro;
	}

	public final void setPreproExp(final int time) {
		this.preproExpiration = time;
	}

	public final Class<? extends Filter> getPreprocessor() {
		return preprocessor;
	}

	public final int getNbSolutions() {
		return nbSolutions;
	}

	protected final void solution() throws IOException {
		nbSolutions++;
		if (resultHandler.isReceiveSolutions()) {
			final Map<IntVariable, Integer> solution = new HashMap<IntVariable, Integer>();
			for (IntVariable v : problem.getVariables()) {
				solution.put(v, v.getValue(v.getFirst()));
			}

			resultHandler.solution(solution, 0, false);
		}
	}

	protected final ResultHandler getResultHandler() {
		return resultHandler;
	}

	public final Problem getProblem() {
		return problem;
	}

	public final boolean preprocess(final Filter filter)
			throws InstantiationException, IllegalAccessException,
			InvocationTargetException, NoSuchMethodException,
			InterruptedException {

		logger.info("Preprocessing (" + preproExpiration + ")");

		final Filter preprocessor;
		if (this.preprocessor == null) {
			preprocessor = filter;
		} else {
			preprocessor = this.preprocessor.getConstructor(Problem.class)
					.newInstance(problem);
		}

		Thread.interrupted();

		final Timer waker = new Timer();

		if (preproExpiration >= 0) {
			waker.schedule(new Waker(Thread.currentThread()),
					preproExpiration * 1000);
		}

		final float start = chronometer.getCurrentChrono();
		boolean consistent;
		try {
			consistent = preprocessor.reduceAll();
		} catch (InterruptedException e) {
			logger.warning("Interrupted preprocessing");
			consistent = true;
			throw e;
		} finally {
			final float preproCpu = chronometer.getCurrentChrono() - start;
			waker.cancel();

			statistics.putAll(preprocessor.getStatistics());

			int removed = 0;

			for (IntVariable v : problem.getVariables()) {
				removed += v.getDomain().maxSize() - v.getDomainSize();

			}
			statistics.put("prepro-removed", removed);
			// statistics("prepro-subs", preprocessor.getNbSub()) ;

			statistics.put("prepro-cpu", preproCpu);
			statistics.put("prepro-constraint-ccks", AbstractConstraint
					.getChecks());
			statistics.put("prepro-constraint-presenceccks", AbstractConstraint
					.getPresenceChecks());
			statistics.put("prepro-matrix2d-ccks", MatrixManager2D.getChecks());
			statistics.put("prepro-matrix2d-presenceccks", MatrixManager2D
					.getPresenceChecks());

			// if (SPACE.BRANCH.equals(space)) {
			// statistics("prepro-singletontests", ((SAC) preprocessor)
			// .getNbSingletonTests());
			// } else if (SPACE.CLASSIC.equals(space)) {
			// statistics("prepro-singletontests", ((AbstractSAC) preprocessor)
			// .getNbSingletonTests());
			// }
		}
		if (!consistent) {
			chronometer.validateChrono();
			return false;
		}
		return true;

	}

	public Map<String, Object> getStatistics() {
		return statistics;
	}

	public static void parameter(final String name, final String value) {
		parameters.put(name, value);
	}

	public String getXMLConfig() {
		final StringBuilder stb = new StringBuilder();

		for (Entry<String, String> p : parameters.entrySet()) {
			stb.append("\t\t\t<p name=\"").append(p.getKey()).append("\">")
					.append(p.getValue()).append("</p>\n");
		}

		return stb.toString();
	}
}
