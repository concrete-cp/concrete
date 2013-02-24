package rb;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Random;

import rb.randomlists.CoarseProportionRandomListGenerator;
import rb.randomlists.ProbabilityRandomListGenerator;
import rb.randomlists.ProportionRandomListGenerator;
import rb.randomlists.RandomListGenerator;
import rb.randomlists.RandomListGenerator.Structure;
import cspfj.generator.FailedGenerationException;
import cspom.CSPOM;
import cspom.constraint.CSPOMConstraint;
import cspom.extension.ExtensionConstraint;
import cspom.extension.HashTrie;
import cspom.extension.HashTrie$;
import cspom.variable.CSPOMVariable;

/**
 * This class corresponds to explicit random problems, i.e., random problems
 * such that constraints are given in extension. <br>
 * 4 2 40 10 10 2 140 0 44 0 0 0 n n <br>
 * 25 10 2 200 0 15 0 0 0 n n 5 8 10 2 22 0 65 0 0 0 n n 25 0 5
 */
public class RBGenerator {

	private static final Random RAND = new Random();

	/**
	 * Nb of variables
	 */
	final private int nbVariables;

	/**
	 * Variable domain Size
	 */
	final private int domainSize;

	/**
	 * Constraint arity
	 */
	final private int arity;

	/**
	 * Nb of constraints
	 */
	final private int nbConstraints;

	/**
	 * Tightness Mode: NBNG, PROPORTION, PROBABILITY, NBSUP
	 */
	public static enum Tightness {
		NBNOGOODS, PROPORTION, PROBABILITY, NBSUPPORTS
	}

	final private Tightness tightnessMode;

	/**
	 * Tightness
	 */
	final private double tightness;

	/**
	 * Seed
	 */
	final private long seed;

	/**
	 * Constraint Graph Type
	 */
	final private Structure constraintGraphType;

	/**
	 * Incompatibility Graph Type
	 */
	final private Structure incompatibilityGraphType;

	/**
	 * Possibility of generating several constraints with same signature
	 */
	final private boolean repetition;

	/**
	 * Generation of satisfiable instances
	 */
	final private boolean alwaysSatisfiable;

	public RBGenerator(int nbVariables, int domainSize, int arity, int nbConstraints,
			Tightness tightnessMode, double tightness, long seed, Structure constraintGraphType,
			Structure incompatibilityGraphType, boolean repetition, boolean alwaysSatisfiable) {
		this.nbVariables = nbVariables;
		this.domainSize = domainSize;
		this.arity = arity;
		this.nbConstraints = nbConstraints;
		this.tightnessMode = tightnessMode;
		this.tightness = tightness;
		this.seed = seed;
		this.constraintGraphType = constraintGraphType;
		this.incompatibilityGraphType = incompatibilityGraphType;
		this.repetition = repetition;
		this.alwaysSatisfiable = alwaysSatisfiable;

	}

	public String getName() {
		final StringBuffer sb = new StringBuffer();
		sb.append("rand-").append(arity);
		sb.append("-").append(nbVariables);
		sb.append("-").append(domainSize);
		sb.append("-").append(nbConstraints);
		final int coeff = tightnessMode == Tightness.PROPORTION
				|| tightnessMode == Tightness.PROBABILITY ? 1000 : 1;
		sb.append("-").append(Math.round(coeff * tightness));
		if (alwaysSatisfiable) {
			sb.append("-fcd");
		}
		sb.append("-").append(seed);
		sb.append("_ext");

		return sb.toString();
	}

	public CSPOM generate() throws FailedGenerationException {
		final CSPOM cspom = new CSPOM();

		final List<CSPOMVariable> variables = new ArrayList<CSPOMVariable>(nbVariables);

		for (int i = nbVariables; --i >= 0;) {
			variables.add(cspom.interVar("X" + i, 0, domainSize - 1));
		}

		RAND.setSeed(seed);
		final Map<CSPOMVariable, Integer> solution = (alwaysSatisfiable ? computeRandomSolution(variables)
				: null);

		final int[] forcedTuple;
		if (alwaysSatisfiable) {
			forcedTuple = new int[arity];
		} else {
			forcedTuple = null;
		}

		final ProportionRandomListGenerator r = new CoarseProportionRandomListGenerator(
				nbVariables, arity, seed);

		final int[][] activeConstraints = r.selectTuples(nbConstraints, constraintGraphType,
				repetition, false);
		for (int i = 0; i < activeConstraints.length; i++) {
			final CSPOMVariable[] involvedVariables = new CSPOMVariable[arity];
			for (int j = 0; j < involvedVariables.length; j++) {
				involvedVariables[j] = variables.get(activeConstraints[i][j]);
				if (alwaysSatisfiable) {
					forcedTuple[j] = solution.get(involvedVariables[j]);
				}
			}
			cspom.addConstraint(buildExplicitConstraint(involvedVariables, tightnessMode,
					tightness, RAND.nextLong(), incompatibilityGraphType, forcedTuple));
		}

		return cspom;

	}

	private Map<CSPOMVariable, Integer> computeRandomSolution(List<CSPOMVariable> variables) {
		Map<CSPOMVariable, Integer> solution = new HashMap<CSPOMVariable, Integer>(nbVariables);
		for (CSPOMVariable v : variables) {
			solution.put(v, RAND.nextInt(v.domain().getValues().size()));
		}
		return solution;
	}

	private long computeNbUnallowedTuplesFrom(CSPOMVariable[] variables, double tightness) {
		long cpt = 1;
		for (int i = variables.length; --i >= 0;) {
			cpt *= variables[i].domain().size();
		}
		return (long) (tightness * cpt);
	}

	private CSPOMConstraint buildExplicitConstraint(CSPOMVariable[] variables,
			Tightness tightnessMode, double tightness, long seed,
			Structure incompatibilityGraphType, int[] forcedTuple) throws FailedGenerationException {
		// System.out.println(tightnessMode);

		final int[] sizes = new int[variables.length];
		for (int i = variables.length; --i >= 0;) {
			sizes[i] = variables[i].domain().size();
		}

		final Extension matrix;

		switch (tightnessMode) {
		case NBNOGOODS:
			matrix = randomMatrix(sizes, (int) tightness, seed, incompatibilityGraphType,
					forcedTuple, false);
			break;

		case NBSUPPORTS:
			matrix = randomMatrix(sizes, (int) tightness, seed, incompatibilityGraphType,
					forcedTuple, true);
			break;

		case PROBABILITY:
			matrix = randomMatrix(sizes, tightness, seed, forcedTuple);
			break;

		default:
			matrix = randomMatrix(sizes, computeNbUnallowedTuplesFrom(variables, tightness), seed,
					incompatibilityGraphType, forcedTuple);
		}

		return new ExtensionConstraint(matrix.e, matrix.sup, variables);
	}

	private static class Extension {
		final HashTrie e;
		final boolean sup;

		public Extension(HashTrie e, boolean sup) {
			this.e = e;
			this.sup = sup;
		}
	}

	private static Extension randomMatrix(int[] sizes, int nbTuples, long seed, Structure type,
			int[] forcedSupport, boolean supports) throws FailedGenerationException {

		ProportionRandomListGenerator r = new CoarseProportionRandomListGenerator(sizes, seed);

		return new Extension(tuplesToMatrix(sizes.length,
				r.selectTuples(nbTuples, type, false, true, forcedSupport, supports)), supports);
	}

	public static Extension randomMatrix(int[] sizes, double nbUnallowedTuples, long seed,
			Structure type, int[] requiredSupport) throws FailedGenerationException {

		final double nbAllowedTuples = RandomListGenerator.computeNbArrangementsFrom(sizes)
				- nbUnallowedTuples;

		// System.out.println("nbAllowedc = " + nbAllowedTuples + " nbUnaloowed
		// = " + nbUnallowedTuples);
		if (nbAllowedTuples > Integer.MAX_VALUE && nbUnallowedTuples > Integer.MAX_VALUE) {
			throw new IllegalArgumentException(
					"The number of allowed and unallowed tuples is greater than Integer.MAX_INT");
		}

		final boolean supports = nbAllowedTuples < nbUnallowedTuples;

		final int nbTuples;
		if (supports) {
			nbTuples = (int) nbAllowedTuples;
		} else {
			nbTuples = (int) nbUnallowedTuples;
		}

		ProportionRandomListGenerator r = new CoarseProportionRandomListGenerator(sizes, seed); // new
		// FineProportionRandomListGenerator(nbValues,
		// seed);

		return new Extension(tuplesToMatrix(sizes.length,
				r.selectTuples(nbTuples, type, false, true, requiredSupport, supports)), supports);
	}

	public static Extension randomMatrix(int[] sizes, double tightness, long seed,
			int[] requiredSupport) throws FailedGenerationException {
		// we assume that each variable has the same domain
		int nbValues = sizes[0];
		int tupleLength = sizes.length;

		double selectionLimit = Math.min(tightness, 1 - tightness);
		ProbabilityRandomListGenerator r = new ProbabilityRandomListGenerator(nbValues,
				tupleLength, seed);
		final boolean supports = tightness > 0.5;
		return new Extension(tuplesToMatrix(sizes.length,
				r.selectTuples(selectionLimit, true, requiredSupport, supports)), supports);

	}

	private static HashTrie tuplesToMatrix(int arity, int[][] tuples) {
		HashTrie extension = HashTrie$.MODULE$.empty();

		for (int[] tuple : tuples) {
			extension = extension.$plus(tuple);
		}
		return extension;
	}

}