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
import java.util.logging.Logger;

import cspfj.constraint.AbstractMatrixManager;
import cspfj.constraint.Constraint;
import cspfj.exception.MaxBacktracksExceededException;
import cspfj.filter.BackedFilter;
import cspfj.filter.Filter;
import cspfj.problem.Problem;
import cspfj.problem.Variable;
import cspfj.util.Chronometer;

public abstract class AbstractSolver implements Solver {

	protected final Problem problem;

	protected final Chronometer chronometer;

	private int nbAssignments;

	private final Map<Variable, Integer> solution;

	private int maxBacktracks;

	private int nbBacktracks;

	private int nbSolutions = 0;

	private final ResultHandler resultHandler;

	private Class<? extends Filter> preprocessor = null;

	private final static Logger logger = Logger
			.getLogger("cspfj.solver.AbstractSolver");

	protected final Map<String, Object> statistics;

	public AbstractSolver(Problem prob, ResultHandler resultHandler) {
		super();
		problem = prob;
		nbAssignments = 0;
		solution = new HashMap<Variable, Integer>();

		chronometer = new Chronometer();
		this.resultHandler = resultHandler;
		this.statistics = new HashMap<String, Object>();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see cspfj.Solver#getSolutionIndex(int)
	 */
	public final int getSolutionValue(final int vId) {
		return getSolution().get(problem.getVariable(vId));
	}

	// /*
	// * (non-Javadoc)
	// *
	// * @see cspfj.Solver#getSolutionValue(int)
	// */
	// public final int getSolutionValue(final int vId) {
	// return problem.getVariable(vId).getDomain()[getSolutionIndex(vId)];
	// }

	public int getNbAssignments() {
		return nbAssignments;
	}

	protected final void incrementNbAssignments() {
		nbAssignments++;
	}

	protected final void addSolutionElement(final Variable variable,
			final int index) {
		solution.put(variable, variable.getDomain()[index]);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see cspfj.Solver#getSolution()
	 */
	public final Map<Variable, Integer> getSolution() {
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

	protected final void setSolution(final Map<Variable, Integer> solution) {
		this.solution.clear();
		this.solution.putAll(solution);
		nbSolutions = 1;
	}

	protected void solution(final Map<Variable, Integer> solution,
			final int nbConflicts) throws IOException {
		if (resultHandler.solution(solution, nbConflicts, false)) {
			logger.info("At " + chronometer.getCurrentChrono());
		}
	}

	public final void setUsePrepro(final Class<? extends Filter> prepro) {
		this.preprocessor = prepro;
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
			final Map<Variable, Integer> solution = new HashMap<Variable, Integer>();
			for (Variable v : problem.getVariables()) {
				solution.put(v, v.getDomain()[v.getFirst()]);
			}

			resultHandler.solution(solution, problem.getNbConstraints(), false);
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
			InvocationTargetException, NoSuchMethodException {

		final Filter preprocessor;
		if (this.preprocessor == null) {
			preprocessor = filter;
		} else if (BackedFilter.class.isAssignableFrom(this.preprocessor)) {
			preprocessor = this.preprocessor.getConstructor(
					new Class[] { Problem.class, Filter.class }).newInstance(
					new Object[] { problem, filter });
		} else {
			preprocessor = this.preprocessor.getConstructor(
					new Class[] { Problem.class }).newInstance(
					new Object[] { problem });
		}

		final float start = chronometer.getCurrentChrono();
		final boolean consistent = preprocessor.reduceAll(0);
		final float preproCpu = chronometer.getCurrentChrono();

		statistics.putAll(preprocessor.getStatistics());

		int removed = 0;

		for (Variable v : problem.getVariables()) {
			removed += v.getDomain().length - v.getDomainSize();

		}
		statistics.put("prepro-removed", removed);
		// statistics("prepro-subs", preprocessor.getNbSub()) ;

		statistics.put("prepro-cpu", preproCpu - start);
		statistics.put("prepro-constraint-ccks", Constraint.getChecks());
		statistics.put("prepro-constraint-presenceccks", Constraint.getPresenceChecks());
		statistics.put("prepro-matrix-ccks", AbstractMatrixManager.getChecks());
		statistics.put("prepro-matrix-presenceccks", AbstractMatrixManager
				.getPresenceChecks());

		// if (SPACE.BRANCH.equals(space)) {
		// statistics("prepro-singletontests", ((SAC) preprocessor)
		// .getNbSingletonTests());
		// } else if (SPACE.CLASSIC.equals(space)) {
		// statistics("prepro-singletontests", ((AbstractSAC) preprocessor)
		// .getNbSingletonTests());
		// }

		if (!consistent) {
			chronometer.validateChrono();
			return false;
		}
		return true;
	}

	public Map<String, Object> getStatistics() {
		return statistics;
	}
}
