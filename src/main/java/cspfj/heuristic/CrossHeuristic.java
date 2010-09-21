package cspfj.heuristic;

import java.lang.reflect.InvocationTargetException;

import cspfj.ParameterManager;
import cspfj.problem.Problem;
import cspfj.problem.Variable;

public final class CrossHeuristic implements Heuristic {

	private final VariableHeuristic variableHeuristic;

	private final ValueHeuristic valueHeuristic;

	static {
		ParameterManager.registerClass("heuristic.variable", WDegOnDom.class);
		ParameterManager.registerClass("heuristic.value", Lexico.class);
	}

	// private final static Logger logger =
	// Logger.getLogger("cspfj.CrossHeuristic");

	public CrossHeuristic(Problem problem) throws InstantiationException,
			IllegalAccessException, InvocationTargetException,
			NoSuchMethodException, ClassNotFoundException {
		variableHeuristic = ((Class<VariableHeuristic>) ParameterManager
				.getParameter("heuristic.variable")).getConstructor(
				Problem.class).newInstance(problem);
		valueHeuristic = ((Class<ValueHeuristic>) ParameterManager
				.getParameter("heuristic.value")).getConstructor()
				.newInstance();
	}

	public Pair selectPair(final Problem problem) {
		final Variable bestVariable = variableHeuristic.selectVariable();
		if (bestVariable == null) {
			return null;
		}
		return new Pair(bestVariable, valueHeuristic.selectIndex(bestVariable));
	}

	public void compute() {
		// logger.fine("Initializing heuristics");
		valueHeuristic.compute();
	}

	public String toString() {
		return "Crossed " + variableHeuristic + ", " + valueHeuristic;
	}
}
