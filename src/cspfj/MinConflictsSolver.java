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
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.Random;
import java.util.Set;
import java.util.TreeSet;
import java.util.logging.Level;
import java.util.logging.Logger;

import cspfj.constraint.Constraint;
import cspfj.exception.MaxBacktracksExceededException;
import cspfj.filter.AC3_P;
import cspfj.filter.Filter;
import cspfj.filter.SAC;
import cspfj.heuristic.Pair;
import cspfj.problem.Problem;
import cspfj.problem.Variable;
import cspfj.util.RandomOrder;
import cspfj.util.TieManager;

public final class MinConflictsSolver extends AbstractSolver {

	private final static Random random = new Random(0);

	private final static Logger logger = Logger
			.getLogger("cspfj.solver.MinConflictsSolver");

	// private final TabuManager tabuManager;

	private final boolean max;

	private final static boolean FINE = logger.isLoggable(Level.FINE);

	private final int[] flipCounter;

	private final TieManager tieManager;

	public MinConflictsSolver(Problem prob, ResultHandler resultHandler) {
		super(prob, resultHandler);

		max = resultHandler.displaySolutions;

		// tabuManager = new TabuManager(problem, 10);

		flipCounter = new int[prob.getMaxVId() + 1];
		Arrays.fill(flipCounter, 0);
		tieManager = new TieManager(random);

	}

	private int realConflicts() {
		int realConflicts = 0;

		for (Constraint c : problem.getConstraints()) {
			if (!c.checkFirst()) {
				realConflicts++;
			}
		}

		return realConflicts;
	}

	private int weightedConflicts() {
		int weightedConflicts = 0;

		for (Constraint c : problem.getConstraints()) {
			if (!c.checkFirst()) {
				weightedConflicts += c.getWeight();
			}
		}

		return weightedConflicts;
	}

	private void solution(final int nbConflicts) throws IOException {
		final Map<Variable, Integer> solution = new HashMap<Variable, Integer>();
		for (Variable v : problem.getVariables()) {
			solution.put(v, v.getDomain()[v.getFirst()]);
		}
		solution(solution, problem.getNbConstraints() - nbConflicts);
	}

	private void init(final Variable variable) {
		if (variable.isAssigned()) {
			return;
		}

		final int index = variable.getBestInitialIndex(tieManager);

		// variable.restoreLevel(1);

		if (FINE) {
			logger.fine(variable.toString() + " (" + variable.getDomainSize()
					+ ") <- " + index);
		}
		variable.firstAssign(index);

		final Set<Variable> randomOrder = new TreeSet<Variable>(
				new RandomOrder<Variable>(random));

		for (Variable n : variable.getNeighbours()) {
			randomOrder.add(n);
		}

		for (Variable v : randomOrder) {
			init(v);
		}
	}

	private void init() {
		final Set<Variable> randomOrder = new TreeSet<Variable>(
				new RandomOrder<Variable>(random));

		for (Variable v : problem.getVariables()) {
			randomOrder.add(v);
		}

		for (Variable v : randomOrder) {
			init(v);
		}

		for (Variable v : problem.getVariables()) {
			v.initNbConflicts(tieManager);
		}
	}

	// private int randomWalk() {
	// final Variable variable = problem.getVariable(random.nextInt(problem
	// .getNbVariables()));
	//
	// final int index = variable.getRandomPresentIndex(random);
	//
	// final int improvment = variable.getImprovment(index);
	//
	// variable.reAssign(index);
	// if (FINE) {
	// logger.fine(variable + " <- " + index);
	// }
	// return improvment;
	// }

	private long findBest(final TieManager tieManager) {
		Variable bestVariable = null;

		tieManager.clear();

		int bestImp = tieManager.getBestEvaluation();

		for (Variable v : problem.getVariables()) {
			if (v.getCurrentConflicts() <= -bestImp) {
				continue;
			}

			final int index = v.bestImprovment();
			if (index < 0) {
				continue;
			}

			if (tieManager.newValue(index, v.getImprovment(index))) {
				bestVariable = v;
				bestImp = tieManager.getBestEvaluation();
			}

		}

		if (bestVariable == null) {
			return -1;
		}
		flipCounter[bestVariable.getId()]++;
		return Pair.pair(bestVariable, tieManager.getBestValue(), problem);

	}

	private int localMinimum() {
		int improvment = 0;

		if (FINE) {
			logger.fine("Local Minimum");
		}

		for (Constraint c : problem.getConstraints()) {
			if (!c.checkFirst()) {
				improvment++;
				c.increaseWeightAndUpdate(tieManager);
			}
		}

		return improvment;
	}

	private int bestWalk(final TieManager tieManager) {

		final long pair = findBest(tieManager);

		// if (pair < 0) {
		// logger.fine("Cleaning tabu list");
		// tabuManager.clean();
		// return 0;
		// }

		final int bestVariableId = Pair.variableId(pair, problem);
		final Variable bestVariable = problem.getVariable(bestVariableId);

		final int bestIndex = Pair.index(pair, problem);

		int improvment = 0;

		if (bestVariable.getImprovment(bestIndex) >= 0) {
			return localMinimum();
		}

		// tabuManager.push(bestVariableId, bestIndex);

		improvment += bestVariable.getImprovment(bestIndex);

		if (FINE) {
			logger.fine(bestVariable + " <- " + bestIndex);
		}
		bestVariable.reAssign(bestIndex, tieManager);
		incrementNbAssignments();
		return improvment;
	}

	public void minConflicts() throws MaxBacktracksExceededException,
			IOException {

		final Problem problem = this.problem;
		// final float randomWalk = this.rWProba;

		init();

		int nbConflicts = weightedConflicts();

		logger.fine("Searching...");

		int bestEver = Integer.MAX_VALUE;

		while (nbConflicts > 0) {
			if (nbConflicts < bestEver) {
				bestEver = nbConflicts;
				solution(nbConflicts);
			}

			if (FINE) {
				logger.fine(nbConflicts + " conflicts " + "(real = "
						+ realConflicts() + ", " + getNbBacktracks() + "/"
						+ getMaxBacktracks() + ")");
			}

			assert realConflicts() <= nbConflicts;

			// if (random.nextFloat() < randomWalk) {
			// nbConflicts += randomWalk();
			// } else {
			nbConflicts += bestWalk(tieManager);
			// }

			assert nbConflicts == weightedConflicts() : nbConflicts + "/="
					+ weightedConflicts() + " (real = " + realConflicts() + ")";

			checkBacktracks();

		}

		for (Variable v : problem.getVariables()) {
			addSolutionElement(v, v.getFirst());
		}

		incrementNbSolutions();
		assert realConflicts() == 0 : getSolution() + " -> " + realConflicts()
				+ " conflicts ! (" + weightedConflicts() + " wc)";

	}

	public boolean runSolver() throws IOException {
		final int localBT = 55000;//problem.getMaxFlips();
		boolean resolved = false;
		System.gc();
		chronometer.startChrono();

		final Filter filter = new AC3_P(problem);

		if (!max) {
			final Filter preprocessor;
			switch (useSpace()) {
			case BRANCH:
				preprocessor = new SAC(problem, filter, true);
				break;

			case CLASSIC:
				preprocessor = new SAC(problem, filter, false);
				break;

			default:
				preprocessor = filter;
			}
			if (!preprocessor.reduceAll(0)) {
				chronometer.validateChrono();
				return false;
			}
		}

		do {
			setMaxBacktracks(localBT);
			logger.info("Run with " + localBT + " flips");
			final int nbAssign = getNbAssignments();
			float localTime = -chronometer.getCurrentChrono();
			try {
				minConflicts();
				resolved = true;
			} catch (MaxBacktracksExceededException e) {
				problem.restoreAll(1);
			} catch (OutOfMemoryError e) {
				chronometer.validateChrono();
				throw e;
			} catch (IOException e) {
				chronometer.validateChrono();
				throw e;
			}
			localTime += chronometer.getCurrentChrono();
			// if (true) {
			// logger.info(problem.toString());
			// chronometer.validateChrono();
			// throw new IOException();
			// }

			// final Set<Constraint> set = new TreeSet<Constraint>(new
			// Weight(false)) ;
			// set.addAll(Arrays.asList(problem.getConstraints()));

			final StringBuffer sb = new StringBuffer();

			for (Variable v : problem.getVariables()) {
				int w = 0;
				for (Constraint c : v.getInvolvingConstraints()) {
					w += c.getWeight();
				}
				sb.append(v).append(" : ").append(w).append(", ").append(
						flipCounter[v.getId()]).append('\n');
			}

			sb.append('\n');

			for (Constraint c : problem.getConstraints()) {
				sb.append(c).append(" : ").append(c.getWeight()).append('\n');
				c.setWeight(Math.max(1, c.getWeight()
						/ problem.getNbConstraints()));
			}
			logger.info(sb.toString());
			logger.info("Took " + localTime + " s (" + (localBT / localTime)
					+ " flips per second), " + (getNbAssignments() - nbAssign)
					+ " assignments made");
			// localBT *= 1.5;
		} while (!resolved);

		chronometer.validateChrono();
		return true;

	}

	public static Random getRandom() {
		return random;
	}

	public String getXMLConfig() {
		final StringBuffer sb = new StringBuffer(200);

		sb.append("\t\t\t<solver>").append(this).append(
				"</solver>\n\t\t\t<maxCSP>").append(max).append("</maxCSP>\n");
		// "</filter>\n\t\t\t<randomWalk>").append(rWProba).append(
		return sb.toString();
	}

	public String toString() {
		return "min-conflicts";
	}
}
