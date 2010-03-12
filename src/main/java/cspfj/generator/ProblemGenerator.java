package cspfj.generator;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import cspfj.problem.Problem;
import cspfj.problem.IntVariable;
import cspom.CSPOM;
import cspom.variable.CSPOMVariable;
import cspom.variable.CSPOMDomain;

public class ProblemGenerator {

	public static Problem generate(CSPOM cspom) {
		final Problem problem = new Problem();

		final Map<CSPOMVariable, IntVariable> variableMap = new HashMap<CSPOMVariable, IntVariable>();

		for (CSPOMVariable v : cspom.getVariables()) {
			problem.addVariable(v);
		}

		return problem;
	}
}
