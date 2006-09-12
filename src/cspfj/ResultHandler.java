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

import java.io.FileWriter;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.util.Arrays;
import java.util.Map;
import java.util.logging.Logger;

import cspfj.problem.Variable;

public class ResultHandler {

	protected final OutputStreamWriter writer;

	protected Solver solver;

	protected long totalLoad = 0;

	protected float totalSolve = 0;

	protected int sat = 0;

	protected int unsat = 0;

	protected int unknown = 0;

	protected boolean displaySolutions;

	private static final Logger logger = Logger
			.getLogger("cspfj.AbstractResultWriter");

	protected int bestConflicts = Integer.MAX_VALUE;

	public ResultHandler(final boolean displaySolutions) {
		this.writer = new OutputStreamWriter(System.out);
		this.displaySolutions = displaySolutions;
	}

	public ResultHandler(String fileName, final boolean displaySolutions)
			throws IOException {
		this.writer = new FileWriter(fileName);
		this.displaySolutions = displaySolutions;
	}

	public void problem(final String name) throws IOException {
		logger.info("loading : " + name);
	}

	public void load(final Solver solver, final long load) throws IOException {
		this.solver = solver;

		totalLoad += load;

		logger.info("loaded in " + (load / 1.0e9F) + " s");

	}

	public boolean solution(final Map<Variable, Integer> solution,
			final int nbConflicts) throws IOException {
		return solution(solution, nbConflicts, false);
	}

	public boolean solution(final Map<Variable, Integer> solution,
			final int nbConflicts, final boolean forced) throws IOException {
		if (forced || displaySolutions && nbConflicts < bestConflicts) {
			bestConflicts = nbConflicts;
			logger.info(solution.toString() + "(" + nbConflicts + ")");
			return true;
		}
		return false;
	}

	public void fail(final Class solver, final Throwable thrown, final long load)
			throws IOException {
		logger.warning(thrown.toString());
		logger.warning(Arrays.toString(thrown.getStackTrace()));
		if (thrown.getCause() != null) {
			logger.warning(thrown.getCause().toString());
			logger.warning(Arrays.toString(thrown.getCause().getStackTrace()));
		}
		unknown++;

	}

	public void result(final Result result, final Throwable thrown,
			final long ccks) throws IOException {
		increment(result);

		totalSolve += solver.getUserTime();

		if (thrown != null) {
			logger.warning(thrown.toString());
		}

		logger.info(result.toString() + " - " + solver.getUserTime() + " s - "
				+ ccks + " ccks - " + solver.getNbAssignments() + " assgn");

	}

	public void result(final Result result, final long ccks) throws IOException {
		result(result, null, ccks);
	}

	public void close() throws IOException {
		logger.info("Total : " + (totalLoad / 1.0e9F) + " s loading and "
				+ totalSolve + " s solving");
		logger.info("SAT : " + sat + ", UNSAT : " + unsat + ", UNKNOWN : "
				+ unknown);
		writer.close();
	}

	private void increment(final Result result) {
		switch (result) {
		case SAT:
			sat++;
			break;

		case UNSAT:
			unsat++;
			break;

		default:
			unknown++;
		}
	}

	public void statistics(final String name, final String value) {
		logger.info(name + " : " + value);
	}

	public enum Result {
		SAT, UNSAT, UNKNOWN
	}

	public final int getSat() {
		return sat;
	}

	public final int getUnknown() {
		return unknown;
	}

	public final int getUnsat() {
		return unsat;
	}

}
