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
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Timer;
import java.util.Map.Entry;
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
import cspfj.util.Waker;

public abstract class AbstractSolver implements Solver {
	private static final Logger LOGGER = Logger.getLogger(AbstractSolver.class
			.getName());
	public static final String VERSION;
	public static final Map<String, String> PARAMETERS = new HashMap<String, String>();
	static {
		Matcher matcher = Pattern.compile("Rev:\\ (\\d+)").matcher(
				"$Rev$");
		matcher.find();
		VERSION = matcher.group(1);
	}

	public static void parameter(final String name, final String value) {
		PARAMETERS.put(name, value);
	}

	protected final Problem problem;

	private int nbAssignments;

	private int maxBacktracks;

	private int nbBacktracks;

	private Class<? extends Filter> preprocessor = null;

	private int preproExpiration = -1;

	private final Map<String, Object> statistics;

	public AbstractSolver(final Problem prob) {
		super();
		problem = prob;
		nbAssignments = 0;
		this.statistics = new HashMap<String, Object>();
	}

	public final int getNbAssignments() {
		return nbAssignments;
	}

	protected final void incrementNbAssignments() {
		nbAssignments++;
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

	public final void setUsePrepro(final Class<? extends Filter> prepro) {
		this.preprocessor = prepro;
	}

	public final void setPreproExp(final int time) {
		this.preproExpiration = time;
	}

	public final Class<? extends Filter> getPreprocessor() {
		return preprocessor;
	}

	protected final Map<String, Integer> solution() throws IOException {
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
		if (this.preprocessor == null) {
			preprocessor = filter;
		} else {
			try {
				preprocessor = this.preprocessor.getConstructor(Problem.class)
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
			System.err.println(e);
			e.printStackTrace();
			throw e;
		} finally {
			preproCpu += System.currentTimeMillis();
			waker.cancel();

			statistics.putAll(preprocessor.getStatistics());

			int removed = 0;

			for (Variable v : problem.getVariables()) {
				removed += v.getDomain().maxSize() - v.getDomainSize();

			}
			statistics.put("prepro-removed", removed);
			statistics.put("prepro-cpu", preproCpu / 1000f);
			statistics.put("prepro-constraint-ccks", AbstractAC3Constraint
					.getChecks());
			statistics.put("prepro-constraint-presenceccks", AbstractConstraint
					.getPresenceChecks());
			statistics.put("prepro-matrix2d-ccks", MatrixManager2D.getChecks());
			statistics.put("prepro-matrix2d-presenceccks", MatrixManager2D
					.getPresenceChecks());

		}

		return consistent;

	}

	public final Map<String, Object> getStatistics() {
		return statistics;
	}

	public String getXMLConfig() {
		final StringBuilder stb = new StringBuilder();

		for (Entry<String, String> p : PARAMETERS.entrySet()) {
			stb.append("\t\t\t<p name=\"").append(p.getKey()).append("\">")
					.append(p.getValue()).append("</p>\n");
		}

		return stb.toString();
	}

	public final void statistic(final String key, final Object value) {
		statistics.put(key, value);
	}

	public final void increaseStatistic(final String key, final Integer value) {
		Object currentValue = statistics.get(key);
		if (currentValue == null) {
			statistics.put(key, value);
		} else if (!(currentValue instanceof Integer)) {
			throw new IllegalArgumentException(value + " should be an integer");
		} else {
			statistics.put(key, (Integer) currentValue + value);
		}
	}

	public final void increaseStatistic(final String key, final Float value) {
		Object currentValue = statistics.get(key);
		if (currentValue == null) {
			statistics.put(key, value);
		} else if (!(currentValue instanceof Float)) {
			throw new IllegalArgumentException(value + " should be a double");
		} else {
			statistics.put(key, (Float) currentValue + value);
		}
	}

}
