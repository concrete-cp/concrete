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
import cspfj.problem.Problem;
import cspfj.problem.Variable;
import cspfj.util.RandomOrder;
import cspfj.util.TieManager;

public abstract class AbstractLocalSolver extends AbstractSolver {

	final private static Random random = new Random(0);

	final private static Logger logger = Logger
			.getLogger("cspfj.AbstractLocalSolver");

	protected static final boolean FINER = logger.isLoggable(Level.FINER);

	private final int[] flipCounter;

	private final TieManager tieManager;

	final private boolean max;

	final protected WCManager[] wcManagers;

	public AbstractLocalSolver(Problem prob, ResultHandler resultHandler) {
		super(prob, resultHandler);

		max = resultHandler.displaySolutions;

		// tabuManager = new TabuManager(problem, 10);

		flipCounter = new int[prob.getMaxVId() + 1];
		Arrays.fill(flipCounter, 0);
		tieManager = new TieManager(random);

		wcManagers = new WCManager[prob.getMaxVId() + 1];
		for (Variable v : prob.getVariables()) {
			wcManagers[v.getId()] = new WCManager(v, tieManager);
		}

	}

	public static Random getRandom() {
		return random;
	}

	protected int realConflicts() {
		int realConflicts = 0;

		for (Constraint c : problem.getConstraints()) {
			if (!c.checkFirst()) {
				realConflicts++;
			}
		}

		return realConflicts;
	}

	protected int weightedConflicts() {
		int weightedConflicts = 0;

		for (Constraint c : problem.getConstraints()) {
			if (!c.checkFirst()) {
				weightedConflicts += c.getWeight();
			}
		}

		return weightedConflicts;
	}

	protected void solution(final int nbConflicts) throws IOException {
		final Map<Variable, Integer> solution = new HashMap<Variable, Integer>();
		for (Variable v : problem.getVariables()) {
			solution.put(v, v.getDomain()[v.getFirst()]);
		}
		solution(solution, problem.getNbConstraints() - nbConflicts);
		
	}

	private void init(final WCManager wcManager) {
		final Variable variable = wcManager.getVariable();

		if (variable.isAssigned()) {
			return;
		}

		wcManager.assignBestInitialIndex();

		if (FINER) {
			logger.finer(variable.toString() + " (" + variable.getDomainSize()
					+ ") <- " + variable.getFirst());
		}

		final Set<WCManager> randomOrder = new TreeSet<WCManager>(
				new RandomOrder<WCManager>(random));

		for (Variable n : variable.getNeighbours()) {
			randomOrder.add(wcManagers[n.getId()]);
		}

		for (WCManager wcm : randomOrder) {
			init(wcm);
		}
	}

	protected void init() {
		final Set<WCManager> randomOrder = new TreeSet<WCManager>(
				new RandomOrder<WCManager>(random));

		for (Variable v : problem.getVariables()) {
			randomOrder.add(wcManagers[v.getId()]);
		}

		for (WCManager v : randomOrder) {
			init(v);
		}

		for (WCManager wcm : randomOrder) {
			wcm.initNbConflicts();
		}
	}

	public abstract void minConflicts() throws MaxBacktracksExceededException,
			IOException;

	public boolean runSolver() throws IOException {
		final int localBT = problem.getMaxFlips();
		boolean resolved = false;
		System.gc();
		chronometer.startChrono();
		if (!max) {
			final Filter filter = new AC3_P(problem);

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
		// int nbTries = 0;
		do {
			// if (nbTries++ > 50) {
			// System.exit(0);
			// }
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

			if (logger.isLoggable(Level.FINE)) {
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
					sb.append(c).append(" : ").append(c.getWeight()).append(
							'\n');

				}
				logger.fine(sb.toString());

			}
			logger.info("Took " + localTime + " s (" + (localBT / localTime)
					+ " flips per second), " + (getNbAssignments() - nbAssign)
					+ " assignments made");
			for (Constraint c : problem.getConstraints()) {
				c.setWeight(1);// Math.max(1, c.getWeight()
				// / problem.getNbConstraints()));
			}

			for (Variable v : problem.getVariables()) {
				v.resetAssign();
			}
			// localBT *= 1.5;
		} while (!resolved);

		chronometer.validateChrono();
		return true;

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

	protected void reAssign(final WCManager wmc, final int index) {
		wmc.reAssign(index);
		final Variable variable = wmc.getVariable();
		for (Constraint c : variable.getInvolvingConstraints()) {
			for (Variable n : c.getInvolvedVariables()) {
				if (n != variable) {
					wcManagers[n.getId()].update(c);
				}
			}
		}
	}

	protected TieManager getTieManager() {
		return tieManager;
	}

	protected void flipped(final Variable variable) {
		flipCounter[variable.getId()]++;
	}

}