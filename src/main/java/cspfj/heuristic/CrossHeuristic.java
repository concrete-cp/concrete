package cspfj.heuristic;

import java.lang.reflect.InvocationTargetException;

import cspfj.ParameterManager;
import cspfj.problem.Problem;
import cspfj.problem.Variable;
import cspfj.util.Parameter;

public final class CrossHeuristic implements Heuristic {

	@Parameter("heuristic.variable")
	private static Class<? extends VariableHeuristic> variableHeuristicClass = WDegOnDom.class;

	@Parameter("heuristic.value")
	private static Class<? extends ValueHeuristic> valueHeuristicClass = Lexico.class;

	private final VariableHeuristic variableHeuristic;

	private final ValueHeuristic valueHeuristic;

	static {
		ParameterManager.register(CrossHeuristic.class);
	}

	// private final static Logger logger =
	// Logger.getLogger("cspfj.CrossHeuristic");

	public CrossHeuristic(Problem problem) throws InstantiationException,
			IllegalAccessException, InvocationTargetException,
			NoSuchMethodException, ClassNotFoundException {
		variableHeuristic = variableHeuristicClass
				.getConstructor(Problem.class).newInstance(problem);
		valueHeuristic = valueHeuristicClass.getConstructor().newInstance();
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
