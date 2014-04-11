package abscon.instance.components;

import abscon.instance.Toolkit;
import abscon.instance.intension.PredicateManager;

public class PFunction { 

	protected String name;

	protected String[] formalParameters;

	protected String functionalExpression;

	protected String[] unversalPostfixExpression;
	

	public String getName() {
		return name;
	}

	public String[] getFormalParameters() {
		return formalParameters;
	}

	public String[] getUniversalPostfixExpression() {
		return unversalPostfixExpression;
	}

	public PFunction(String name, String formalParametersExpression, String functionalExpression) {
		this.name = name;
		this.formalParameters =  PredicateManager.extractFormalParameters(formalParametersExpression,true);
		this.functionalExpression = functionalExpression;
		this.unversalPostfixExpression = PredicateManager.buildUniversalPostfixExpression(functionalExpression, formalParameters);
	}

	public String toString() {
		return "  function " + name + " with functional expression = " + functionalExpression + " and (universal) postfix expression = " + Toolkit.buildStringFromTokens(unversalPostfixExpression);
	}
}
