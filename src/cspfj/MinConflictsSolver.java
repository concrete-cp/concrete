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
import java.util.logging.Level;
import java.util.logging.Logger;

import cspfj.constraint.Constraint;
import cspfj.exception.MaxBacktracksExceededException;
import cspfj.problem.Problem;
import cspfj.problem.Variable;
import cspfj.util.TieManager;
import cspfj.filter.AC3_P;
import cspfj.filter.AC3_R;
import cspfj.filter.Filter;
import cspfj.filter.SAC;
import cspfj.heuristic.Heuristic;

public final class MinConflictsSolver extends AbstractSolver {

	private final static Random random = new Random(0);

	// private int nbConflicts;

	private final static Logger logger = Logger
			.getLogger("cspfj.solver.MinConflictsSolver");

	private final boolean[] toUpdate;

	private final TabuManager tabuManager;

	private final float randomWalk;

	private final Filter filter;

	private final boolean max;

	public MinConflictsSolver(Problem prob, ResultHandler resultHandler,
			Heuristic heuristic, boolean reverse) {
		super(prob, resultHandler);
		filter = reverse ? new AC3_R(problem) : new AC3_P(problem);
		toUpdate = new boolean[prob.getMaxVId() + 1];
		Arrays.fill(toUpdate, false);

		tabuManager = new TabuManager(problem, 15);

		randomWalk = 1F / problem.getMaxDomainSize();

		max = resultHandler.displaySolutions;

		Variable.setTieManager(new TieManager(random));

	}

	private void init(Variable variable) {
//		for (Variable v : problem.getVariables()) {
//			if (v.getDomainSize() <= 0) {
//				v.restoreLevel(1);
//			}
//		}
		
		if (variable.isAssigned()) {
			return ;
		}

		final int index = variable.getBestInitialIndex();

		variable.restoreLevel(1);

		if (logger.isLoggable(Level.FINE)) {
			logger.fine(variable.toString() + " (" + variable.getDomainSize() + ") <- "
					+ index);
		}
		variable.firstAssign(index);
		
		for (Variable n : variable.getNeighbours()) {
			init(n) ;
		}
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

	public void minConflicts() throws MaxBacktracksExceededException,
			IOException {


		final Logger logger = MinConflictsSolver.logger ;
		final Random random = MinConflictsSolver.random ;
		final Problem problem = this.problem ;
		final float randomWalk = this.randomWalk;
		final TabuManager tabuManager = this.tabuManager;

		final Variable[] variables = problem.getVariables();
		
		init(variables[random.nextInt(variables.length)]);

		for (Variable v: problem.getVariables()) {
			v.initNbConflicts();
		}
		
		int nbConflicts = weightedConflicts() ;
		
		logger.fine("Searching...");

		int bestEver = Integer.MAX_VALUE;

		final TieManager tieManager = new TieManager(random);

		while (nbConflicts > 0) {
			if (nbConflicts < bestEver) {
				bestEver = nbConflicts;
				final Map<Variable, Integer> solution = new HashMap<Variable, Integer>();
				for (Variable v : problem.getVariables()) {
					solution.put(v, v.getDomain()[v.getFirst()]);
				}
				solution(solution, problem.getNbConstraints() - nbConflicts);
				// incrementNbSolutions();
			}

			if (logger.isLoggable(Level.FINE)) {
				logger.fine(nbConflicts + " conflicts " + "(real = "
						+ realConflicts() + ", " + getNbBacktracks() + "/"
						+ getMaxBacktracks() + ")");

			}
			assert realConflicts() <= nbConflicts;

			if (random.nextFloat() < randomWalk) {
				final Variable variable = problem.getVariable(random
						.nextInt(problem.getNbVariables()));

				final int index = variable.getRandomPresentIndex(random);

				nbConflicts += variable.getImprovment(index);

				variable.reAssign(index);

			} else {

				Variable bestVariable = null;

				tieManager.clear();

				int bestImp = tieManager.getBestEvaluation();

				for (Variable v : problem.getVariables()) {
					// logger.fine(bestImp + "");

					if (v.getCurrentConflicts() <= -bestImp) {
						// logger.info("zap");
						continue;
					}

					final int index = v.bestImprovment(tabuManager, bestEver
							- nbConflicts);
					if (index < 0) {
						continue;
					}

					if (tieManager.newValue(index, v.getImprovment(index))) {
						bestVariable = v;
						bestImp = tieManager.getBestEvaluation();
					}

				}

				final int bestIndex = tieManager.getBestValue();

				if (bestIndex < 0) {
					logger.fine("Cleaning tabu list");
					tabuManager.clean();
					checkBacktracks();
					continue;
				}

				tabuManager.push(bestVariable, bestIndex);

				// bestVariable.unassign(problem);
				final int conflictsBefore = nbConflicts;
				nbConflicts += bestVariable.getImprovment(bestIndex);

				if (logger.isLoggable(Level.FINE)) {
					logger.fine(bestVariable + " <- "
							+ tieManager.getBestValue());
				}
				bestVariable.reAssign(bestIndex);

				if (nbConflicts >= conflictsBefore) {
					// Minimum local
					checkBacktracks();
					for (Constraint c : problem.getConstraints()) {
						if (!c.checkFirst()) {
							nbConflicts++;
							c.increaseWeightAndUpdate();
						}
					}
				}

				// toUpdate[bestVariable.getId()] = true;
				// for (Variable n : bestVariable.getNeighbours()) {
				// toUpdate[n.getId()] = true;
				// }
				//
				// for (int i = 0; i < problem.getNbVariables(); i++) {
				// if (toUpdate[i]) {
				// final Variable variable = problem.getVariable(i);
				// for (Constraint c : variable.getInvolvingConstraints()) {
				// variable.updateNbConflicts(c);
				// }
				//
				// toUpdate[i] = false;
				// }
				// }

			}
			assert nbConflicts == weightedConflicts() : nbConflicts + "/="
					+ weightedConflicts();
			incrementNbAssignments();

		}

		for (Variable v : problem.getVariables()) {
			addSolutionElement(v, v.getFirst());
		}

		incrementNbSolutions();
		assert realConflicts() == 0 : getSolution() + " -> " + realConflicts()
				+ " conflicts ! (" + weightedConflicts() + " wc)";

	}

	public boolean run() throws IOException {
		int localBT = problem.getMaxDomainSize();
		boolean resolved = false;
		System.gc();
		chronometer.startChrono();

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

		do {
			setMaxBacktracks(localBT);
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

			for (Constraint c : problem.getConstraints()) {
				c.setWeight(Math.max(1, (int) Math.sqrt(c.getWeight())));
			}
			localBT *= 1.5;
		} while (!resolved);

		chronometer.validateChrono();
		return true;

	}

	public static Random getRandom() {
		return random;
	}

	public String getXMLConfig() {
		final StringBuffer sb = new StringBuffer();

		sb.append("\t\t\t<solver>").append(this).append("</solver>\n");
		sb.append("\t\t\t<filter>").append(filter).append("</filter>\n");
		sb.append("\t\t\t<randomWalk>").append(randomWalk).append(
				"</randomWalk>\n");
		sb.append("\t\t\t<tabu>").append(tabuManager.getSize()).append(
				"</tabu>\n");
		sb.append("\t\t\t<maxCSP>").append(max).append("</maxCSP>\n");

		return sb.toString();
	}

	public String toString() {
		return "min-conflicts";
	}
}
