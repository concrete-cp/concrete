package abscon.instance.intension.terminal;

import abscon.instance.intension.types.BooleanType;

public class FalseEvaluator extends TerminalEvaluator implements BooleanType{
	public void evaluate() {
		stack[++top] = 0;
	}

}
