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
import cspfj.filter.AC3_P;
import cspfj.problem.Problem;
import cspfj.problem.Variable;
import cspfj.util.RandomOrder;
import cspfj.util.TieManager;

public abstract class AbstractLocalSolver extends AbstractSolver {

	final private static Random random = new Random(0);

	final private static Logger logger = Logger
			.getLogger("cspfj.AbstractLocalSolver");

	protected static final boolean FINER = logger.isLoggable(Level.FINER);

	// private final int[] flipCounter;

	private final TieManager tieManager;

	final private boolean max;

	final protected ConflictsManager[] wcManagers;

	// private Variable bestVariable ;
	//	
	// private int bestImp ;

	public static void setSeed(final long seed) {
		random.setSeed(seed);
	}

	public AbstractLocalSolver(Problem prob, ResultHandler resultHandler) {
		super(prob, resultHandler);

		max = resultHandler.displaySolutions;

		// tabuManager = new TabuManager(problem, 10);

		// flipCounter = new int[prob.getMaxVId() + 1];
		// Arrays.fill(flipCounter, 0);
		tieManager = new TieManager(random);

		wcManagers = new ConflictsManager[prob.getMaxVId() + 1];
		for (Variable v : prob.getVariables()) {
			wcManagers[v.getId()] = new ConflictsManager(v, tieManager);
		}

		setMaxBacktracks(getMaxFlips());
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

	private void init(final ConflictsManager wcManager) {
		final Variable variable = wcManager.getVariable();

		if (variable.isAssigned()) {
			return;
		}

		wcManager.assignBestInitialIndex();

		if (FINER) {
			logger.finer(variable.toString() + " (" + variable.getDomainSize()
					+ ") <- " + variable.getFirst());
		}

		final Set<ConflictsManager> randomOrder = new TreeSet<ConflictsManager>(
				new RandomOrder<ConflictsManager>(random));

		for (Variable n : variable.getNeighbours()) {
			randomOrder.add(wcManagers[n.getId()]);
		}

		for (ConflictsManager wcm : randomOrder) {
			init(wcm);
		}
	}

	protected void init() {
		final Set<ConflictsManager> randomOrder = new TreeSet<ConflictsManager>(
				new RandomOrder<ConflictsManager>(random));

		for (Variable v : problem.getVariables()) {
			randomOrder.add(wcManagers[v.getId()]);
		}

		for (ConflictsManager v : randomOrder) {
			init(v);
		}

		for (ConflictsManager wcm : randomOrder) {
			wcm.initNbConflicts();
		}

		// initBestVariable();
	}

	public abstract void minConflicts() throws MaxBacktracksExceededException,
			IOException;

	public boolean runSolver() throws IOException {
		final int localBT = getMaxBacktracks();
		boolean resolved = false;
		System.gc();
		chronometer.startChrono();
		if (!max && !preprocess(new AC3_P(problem))) {
			return false;
		}
//		int nbTries = 0;
		do {
//			if (nbTries++ > 0) {
//				return false;
//			}
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
				logger.fine(weightStats());
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

	public String weightStats() {
		final StringBuffer sb = new StringBuffer();

		for (int i = 0; i <= problem.getMaxVId(); i++) {
			final Variable v = problem.getVariable(i);
			if (v == null) {
				continue;
			}

			int w = 0;
			for (Constraint c : v.getInvolvingConstraints()) {
				w += c.getWeight();
			}
			sb.append(v).append(" : ").append(w).append(", ").append(
					v.getWeight()).append('\n');
		}

		sb.append('\n');

		for (Constraint c : problem.getConstraints()) {
			sb.append(c).append(" : ").append(c.getWeight()).append('\n');

		}

		return sb.toString();
	}

	public String getXMLConfig() {
		final StringBuffer sb = new StringBuffer(200);

		sb.append("\t\t\t<solver>").append(this).append(
				"</solver>\n\t\t\t<maxCSP>").append(max).append("</maxCSP>\n");
		// "</filter>\n\t\t\t<randomWalk>").append(rWProba).append(
		return sb.toString();
	}

	protected void reAssign(final ConflictsManager vcm, final int index) {
		vcm.reAssign(index);
		final Variable variable = vcm.getVariable();
		final int vId = variable.getId();
		// bestVariable = variable ;
		// bestImp = vcm.getBestImprovment();
		for (Constraint c : variable.getInvolvingConstraints()) {
			final Variable[] involvedVariables = c.getInvolvedVariables();
			for (int n = involvedVariables.length; --n >= 0;) {
				final Variable neighbour = involvedVariables[n];
				final int nId = neighbour.getId() ;
				if (nId != vId) {
					wcManagers[nId].update(c, n);
				}
			}
		}
		// initBestVariable() ;
	}

	protected TieManager getTieManager() {
		return tieManager;
	}

	protected int getMaxFlips() {
		return  problem.getMaxFlips();
	}

	// protected Variable getBestVariable() {
	// return bestVariable ;
	// }
	//	
	// protected int getBestImp(){
	// return bestImp ;
	// }

	// protected void updateBestVariable(final ConflictsManager vcm) {
	// if (vcm.getBestImprovment() < bestImp) {
	// bestVariable = vcm.getVariable() ;
	// bestImp = vcm.getBestImprovment();
	// }
	// }

	// protected void initBestVariable() {
	// bestVariable = wcManagers[0].getVariable() ;
	// bestImp = wcManagers[0].getBestImprovment();
	// final TieManager tieManager = this.tieManager;
	// tieManager.clear();
	// for (ConflictsManager vcm: wcManagers) {
	// if(tieManager.newValue(vcm.getBestImprovment())) {
	// bestVariable = vcm.getVariable() ;
	// bestImp = vcm.getBestImprovment();
	// }
	// }
	// }
}