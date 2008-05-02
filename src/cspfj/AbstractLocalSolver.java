package cspfj;

import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.security.InvalidParameterException;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Random;
import java.util.Set;
import java.util.TreeSet;
import java.util.logging.Level;
import java.util.logging.Logger;

import cspfj.constraint.Constraint;
import cspfj.exception.MaxBacktracksExceededException;
import cspfj.filter.AC3;
import cspfj.problem.ConflictsManager;
import cspfj.problem.Problem;
import cspfj.problem.Variable;
import cspfj.util.RandomOrder;
import cspfj.util.TieManager;

public abstract class AbstractLocalSolver extends AbstractSolver implements LocalSolver {

	final private static Random random = new Random(0);

	final private static Logger logger = Logger
			.getLogger("cspfj.AbstractLocalSolver");

	protected static final boolean FINER = logger.isLoggable(Level.FINER);

	private final TieManager tieManager;

	final private boolean max;

	final protected ConflictsManager[] wcManagers;

	private int maxTries = -1;
	
	private final double[] weights ;

	public static void setSeed(final long seed) {
		random.setSeed(seed);
	}

	public void setMaxTries(final int tries) {
		this.maxTries = tries;
	}

	public AbstractLocalSolver(Problem prob, ResultHandler resultHandler,
			final boolean max) {
		super(prob, resultHandler);

		this.max = max;

		tieManager = new TieManager(random);

		wcManagers = new ConflictsManager[prob.getMaxVId() + 1];
		for (Variable v : prob.getVariables()) {
			wcManagers[v.getId()] = new ConflictsManager(v, tieManager, this);
		}

		setMaxBacktracks(150000);

		weights = new double[problem.getMaxCId()+1] ;
		Arrays.fill(weights, 1);
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

	 protected double weightedConflicts() {
		double weightedConflicts = 0;

		for (Constraint c : problem.getConstraints()) {
			if (!c.checkFirst()) {
				weightedConflicts += weights[c.getId()];
			}
		}

		return weightedConflicts;
	}

	 public void increaseWeight(Constraint c) {
		 weights[c.getId()]++;
	 }
	 
	 public double getWeight(Constraint c) {
		 return weights[c.getId()];
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

		final Set<Variable> neighbours = new HashSet<Variable>();

		for (Constraint c : variable.getInvolvingConstraints()) {
			for (Variable v : c.getInvolvedVariables()) {
				neighbours.add(v);
			}
		}

		for (Variable n : neighbours) {
			if (n != variable) {
				randomOrder.add(wcManagers[n.getId()]);
			}
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

	}

	public abstract void minConflicts() throws MaxBacktracksExceededException,
			IOException;

	public boolean runSolver() throws IOException {
		final int localBT = getMaxBacktracks();
		boolean resolved = false;
		System.gc();
		chronometer.startChrono();
		try {
			if (!max && !preprocess(new AC3(problem, null))) {
				return false;
			}
		} catch (InstantiationException e1) {
			throw new InvalidParameterException(e1.toString());
		} catch (IllegalAccessException e1) {
			throw new InvalidParameterException(e1.toString());
		} catch (InvocationTargetException e1) {
			throw new InvalidParameterException(e1.toString());
		} catch (NoSuchMethodException e1) {
			throw new InvalidParameterException(e1.toString());
		} catch (InterruptedException e) {
			// No problem...
		}
		int nbTries = 0;
		do {
			if (nbTries++ >= maxTries && maxTries > 0) {
				return false;
			}
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

			logger.info("Took " + localTime + " s (" + (localBT / localTime)
					+ " flips per second), " + (getNbAssignments() - nbAssign)
					+ " assignments made");
//			for (Constraint c : problem.getConstraints()) {
//				c.setWeight(1);// Math.max(1, c.getWeight()
//				// / problem.getNbConstraints()));
//			}

			for (Variable v : problem.getVariables()) {
				v.resetAssign();
			}

		} while (!resolved);

		chronometer.validateChrono();
		return true;

	}

	public String getXMLConfig() {
		return new StringBuffer(200).append("\t\t\t<solver>").append(this)
				.append("</solver>\n\t\t\t<maxCSP>").append(max).append(
						"</maxCSP>\n\t\t\t<maxIterations>").append(
						getMaxBacktracks()).append("</maxIterations>\n")
				.toString();
	}

	protected void reAssign(final ConflictsManager vcm, final int index) {
		vcm.reAssign(index);
		final Variable variable = vcm.getVariable();
		final int vId = variable.getId();
		for (Constraint c : variable.getInvolvingConstraints()) {
			final Variable[] involvedVariables = c.getInvolvedVariables();
			for (int n = involvedVariables.length; --n >= 0;) {
				final Variable neighbour = involvedVariables[n];
				final int nId = neighbour.getId();
				if (nId != vId) {
					wcManagers[nId].update(c, n);
				}
			}
		}
	}

	protected TieManager getTieManager() {
		return tieManager;
	}

}