package cspfj;

import java.util.HashMap;
import java.util.Map;

import cspfj.problem.Variable;

public class SolutionHandler {
	private Map<Variable, Integer> solution = null;

	private boolean result = false;

	private Map<Integer, Integer> weights = new HashMap<Integer, Integer>();

	public synchronized Map<Variable, Integer> getSolution()
			throws InterruptedException {
		while (!result) {
			wait();
		}
		return solution;
	}

	public synchronized void setSolution(Map<Variable, Integer> solution) {
		result = true;
		this.solution = solution;
		notifyAll();
	}

	public synchronized void addWeight(int cid, int weight) {
		if (weights.containsKey(cid)) {
			weights.put(cid, weights.get(cid) + weight);
		} else {
			weights.put(cid, weight);
		}
	}

	public synchronized Map<Integer, Integer> getWeights() {
		return weights;
	}

}
