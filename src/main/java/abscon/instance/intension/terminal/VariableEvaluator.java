package abscon.instance.intension.terminal;

import abscon.instance.intension.EvaluationManager;
import abscon.instance.intension.types.IntegerType;

public class VariableEvaluator extends TerminalEvaluator implements IntegerType{

	private EvaluationManager manager;

	private int position;

	public int getPosition() {
		return position;
	}

	public VariableEvaluator(EvaluationManager manager, int position) {
		this.manager = manager;
		this.position = position;
	}

	public void evaluate() {
		stack[++top] = manager.getCurentValueOf(position);
	}

}
