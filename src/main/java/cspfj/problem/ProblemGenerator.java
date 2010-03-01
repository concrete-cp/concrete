package cspfj.problem;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import cspom.CSPOM;
import cspom.variable.CSPOMVariable;
import cspom.variable.CSPOMDomain;

public class ProblemGenerator {

	public static Problem generate(CSPOM cspom) {
		final Problem problem = new Problem();

		final Map<CSPOMVariable, Variable> variableMap = new HashMap<CSPOMVariable, Variable>();

		for (CSPOMVariable v : cspom.getVariables()) {
			problem.addVariable(v);
		}
		return problem;
	}
}
