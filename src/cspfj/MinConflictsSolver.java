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
import java.util.HashMap;
import java.util.Map;
import java.util.Random;
import java.util.Set;
import java.util.TreeSet;
import java.util.logging.Level;
import java.util.logging.Logger;

import cspfj.constraint.Constraint;
import cspfj.exception.MaxBacktracksExceededException;
import cspfj.problem.Problem;
import cspfj.problem.Variable;
import cspfj.util.RandomOrder;
import cspfj.util.TieManager;
import cspfj.filter.AC3_P;
import cspfj.filter.AC3_R;
import cspfj.filter.Filter;
import cspfj.filter.SAC;
import cspfj.heuristic.Pair;

public final class MinConflictsSolver extends AbstractSolver {

	private final static Random random = new Random(0);

	private final static Logger logger = Logger
			.getLogger("cspfj.solver.MinConflictsSolver");

	private final TabuManager tabuManager;

	// private final float rWProba;

	private final Filter filter;

	private final boolean max;

	private final static boolean FINE = logger.isLoggable(Level.FINE);

	public MinConflictsSolver(Problem prob, ResultHandler resultHandler,
			boolean reverse) {
		super(prob, resultHandler);

		max = resultHandler.displaySolutions;

		if (max) {
			filter = null;
		} else {
			filter = reverse ? new AC3_R(problem) : new AC3_P(problem);
		}
		tabuManager = new TabuManager(problem, 10);
	
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

	private void init(final Variable variable, final TieManager tieManager) {
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
			init(v, tieManager);
		}
	}

	private void init(final TieManager tieManager) {
		final Set<Variable> randomOrder = new TreeSet<Variable>(
				new RandomOrder<Variable>(random));

		for (Variable v : problem.getVariables()) {
			randomOrder.add(v);
		}

		for (Variable v : randomOrder) {
			init(v, tieManager);
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

	private long findBest(final TieManager tieManager, final TieManager tieManager2, final int aspiration) {
		Variable bestVariable = null;

		tieManager.clear();

		int bestImp = tieManager.getBestEvaluation();

		for (Variable v : problem.getVariables()) {
			if (v.getCurrentConflicts() <= -bestImp) {
				continue;
			}

			final int index = v.bestImprovment(tabuManager, aspiration, tieManager2);
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
		return Pair.pair(bestVariable, tieManager.getBestValue(), problem);

	}

	private int localMinimum(final TieManager tieManager) {
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

	private int bestWalk(final TieManager tieManager, final TieManager tieManager2, final int aspiration) {

		final long pair = findBest(tieManager, tieManager2, aspiration);

		if (pair < 0) {
			logger.fine("Cleaning tabu list");
			tabuManager.clean();
			return 0;
		}

		final Variable bestVariable = Pair.variable(pair, problem);

		final int bestIndex = Pair.index(pair, problem);

		int improvment = 0;

		if (bestVariable.getImprovment(bestIndex) >= 0) {
			return localMinimum(tieManager2);
		}

		tabuManager.push(bestVariable, bestIndex);

		improvment += bestVariable.getImprovment(bestIndex);

		if (FINE) {
			logger.fine(bestVariable + " <- " + tieManager.getBestValue());
		}
		bestVariable.reAssign(bestIndex, tieManager2);
		incrementNbAssignments();
		return improvment;
	}

	public void minConflicts() throws MaxBacktracksExceededException,
			IOException {

		final Problem problem = this.problem;
		// final float randomWalk = this.rWProba;

		final TieManager tieManager = new TieManager(random);
		
		init(tieManager);

		int nbConflicts = weightedConflicts();

		logger.fine("Searching...");

		int bestEver = Integer.MAX_VALUE;

		final TieManager tieManager2 = new TieManager(random);

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
			nbConflicts += bestWalk(tieManager, tieManager2, bestEver - nbConflicts);
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
		final int localBT = problem.getMaxFlips();
		boolean resolved = false;
		System.gc();
		chronometer.startChrono();

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
//			 if (true) {
//			 logger.info(problem.toString());
//			 chronometer.validateChrono();
//			 throw new IOException();
//			 }
			for (Constraint c : problem.getConstraints()) {
				c.setWeight(1);// Math.max(1, (int) Math.log(c.getWeight())));
			}

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
				"</solver>\n\t\t\t<filter>").append(filter).append(

		"</randomWalk>\n\t\t\t<tabu>").append(tabuManager.getSize()).append(
				"</tabu>\n\t\t\t<maxCSP>").append(max).append("</maxCSP>\n");
		// "</filter>\n\t\t\t<randomWalk>").append(rWProba).append(
		return sb.toString();
	}

	public String toString() {
		return "min-conflicts";
	}
}
