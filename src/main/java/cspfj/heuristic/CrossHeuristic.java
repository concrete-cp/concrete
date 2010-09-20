package cspfj.heuristic;

import java.lang.reflect.InvocationTargetException;

import cspfj.AbstractSolver;
import cspfj.problem.Problem;
import cspfj.problem.Variable;

public final class CrossHeuristic implements Heuristic {

	private final VariableHeuristic variableHeuristic;

	private final ValueHeuristic valueHeuristic;

	static {
		AbstractSolver.defaultParameter("heuristic.variable", "WDegOnDom");
		AbstractSolver.defaultParameter("heuristic.value", "Lexico");
	}

	// private final static Logger logger =
	// Logger.getLogger("cspfj.CrossHeuristic");

	public CrossHeuristic(Problem problem) throws IllegalArgumentException,
			SecurityException, InstantiationException, IllegalAccessException,
			InvocationTargetException, NoSuchMethodException,
			ClassNotFoundException {
		variableHeuristic = (VariableHeuristic) Class
				.forName(AbstractSolver.PARAMETERS.get("heuristic.variable"))
				.getConstructor(Problem.class).newInstance(problem);
		valueHeuristic = (ValueHeuristic) Class
				.forName(AbstractSolver.PARAMETERS.get("heuristic.value"))
				.getConstructor().newInstance();
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
