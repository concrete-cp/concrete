package abscon.instance.intension.terminal;

import abscon.instance.intension.types.BooleanType;

public class TrueEvaluator extends TerminalEvaluator implements BooleanType {
	public void evaluate() {
		stack[++top] = 1;
	}

}
