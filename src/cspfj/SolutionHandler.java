package cspfj;

import java.util.HashMap;
import java.util.Map;

import cspfj.problem.Variable;

public final class SolutionHandler {
	private Map<Variable, Integer> solution = null;

	private boolean result = false;

	private final Map<Integer, Integer> weights = new HashMap<Integer, Integer>();

	public Map<Variable, Integer> getSolution() throws InterruptedException {
		while (!result) {
			synchronized (this) {
				wait();
			}
		}
		return solution;
	}

	public void setSolution(final Map<Variable, Integer> solution) {
		result = true;
		this.solution = solution;
		synchronized (this) {
			notifyAll();
		}
	}

	public void addWeight(final int cid, final int weight) {
		if (weights.containsKey(cid)) {
			weights.put(cid, weights.get(cid) + weight);
		} else {
			weights.put(cid, weight);
		}
	}

	public Map<Integer, Integer> getWeights() {
		return weights;
	}

}
