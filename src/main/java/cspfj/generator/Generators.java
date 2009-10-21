package cspfj.generator;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import cspfj.problem.Problem;
import cspfj.problem.Variable;
import cspom.constraint.CSPOMConstraint;
import cspom.variable.CSPOMVariable;

public class Generators {

	final Map<String, List<ConstraintGenerator>> generators;

	public Generators() {
		generators = new HashMap<String, List<ConstraintGenerator>>();
		addGenerator(new ReifiedNeqGenerator());
	}

	public void addGenerator(ConstraintGenerator generator) {
		List<ConstraintGenerator> candidates = generators.get(generator
				.getDescription());

		if (candidates == null) {
			candidates = new LinkedList<ConstraintGenerator>();
			generators.put(generator.getDescription(), candidates);
		}

		candidates.add(generator);

	}

	public ConstraintGenerator findGenerator(CSPOMConstraint constraint) {
		final List<ConstraintGenerator> candidates = generators.get(constraint
				.getDescription());
		if (candidates == null) {
			return null;
		}

		for (ConstraintGenerator cg : candidates) {
			if (cg.isGenerable(constraint)) {
				return cg;
			}
		}

		return null;
	}

	public boolean generate(Problem problem,
			Map<CSPOMVariable, Variable> variableMap, CSPOMConstraint constraint) {
		final ConstraintGenerator generator = findGenerator(constraint);
		if (generator == null) {
			return false;
		}

		return true;

	}

	// public List<ConstraintTransformer> transformers() {
	// final List<ConstraintTransformer> transformers = new
	// ArrayList<ConstraintTransformer>();
	//
	// transformers.add(new ConstraintTransformer() {
	//
	// @Override
	// public void generate(CSPOM sourceSubProblem) {
	// final FunctionalConstraint cspomConstraint = (FunctionalConstraint)
	// sourceSubProblem
	// .getConstraints().iterator().next();
	//
	// Variable res = var(cspomConstraint.getResultVariable());
	// Variable[] args = new Variable[2];
	// for (int i = 2; --i >= 0;) {
	// args[i] = var(cspomConstraint.getArguments()[i]);
	// }
	//
	// problem.addConstraint(new ReifiedNeqConstraint(res, args[0],
	// args[1]));
	//
	// }
	//
	// @Override
	// public CSPOM pattern() {
	// final CSPOM cspom = new CSPOM();
	// try {
	// cspom.addVariable(new CSPOMVariable("v0",
	// new UnknownDomain(Type.INTEGER)));
	// cspom.addVariable(new CSPOMVariable("v1",
	// new UnknownDomain(Type.INTEGER)));
	// cspom.addVariable(new CSPOMVariable("r",
	// new BooleanDomain()));
	// } catch (DuplicateVariableException e) {
	// throw new IllegalStateException(e);
	// }
	// try {
	// cspom.ctr("r = neq(v0, v1)");
	// } catch (ParseException e) {
	// throw new IllegalStateException(e);
	// }
	// return cspom;
	// }
	//
	// });
	//
	// return transformers;
	// }
}
