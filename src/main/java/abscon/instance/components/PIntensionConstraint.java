package abscon.instance.components;

import abscon.instance.Toolkit;
import abscon.instance.intension.EvaluationManager;
import abscon.instance.intension.PredicateManager;

public class PIntensionConstraint extends PConstraint {
	private PFunction function; // a predicate is a kind of function - so function may be a PPredicate

	private String[] universalPostfixExpression;

	public PFunction getFunction() {
		return function;
	}

	public String[] getUniversalPostfixExpression() {
		return universalPostfixExpression;
	}

	public PIntensionConstraint(String name, PVariable[] scope, PFunction function, String effectiveParametersExpression) {
		super(name, scope);
		this.function = function;

		String[] variableNames = new String[scope.length];
		for (int i = 0; i < variableNames.length; i++)
			variableNames[i] = scope[i].getName();

		this.universalPostfixExpression = PredicateManager.buildNewUniversalPostfixExpression(function.getUniversalPostfixExpression(), effectiveParametersExpression, variableNames);
		// System.out.println(universalPredicateExpression);
	}

	public long computeCostOf(int[] tuple) {
		EvaluationManager evaluationManager = new EvaluationManager(universalPostfixExpression);
		long result = evaluationManager.evaluate(tuple);
		if (function instanceof PPredicate) {
			boolean satisfied = result == 1;
			return satisfied ? 0 : 1;
		}
		return result;
	}

	public String toString() {
		return super.toString() + ", and associated function/predicate " + function.getName() + " and universal expression = " + Toolkit.buildStringFromTokens(universalPostfixExpression);
	}

	public boolean isGuaranteedToBeDivisionByZeroFree() {
		EvaluationManager evaluationManager = new EvaluationManager(universalPostfixExpression);
		return evaluationManager.isGuaranteedToBeDivisionByZeroFree(scope);
	}

	public boolean isGuaranteedToBeOverflowFree() {
		// System.out.println("cons " + name);
		EvaluationManager evaluationManager = new EvaluationManager(universalPostfixExpression);
		return evaluationManager.isGuaranteedToBeOverflowFree(scope);
	}
}
