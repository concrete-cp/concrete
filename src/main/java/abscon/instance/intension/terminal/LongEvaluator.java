package abscon.instance.intension.terminal;

import abscon.instance.intension.types.IntegerType;

public class LongEvaluator extends TerminalEvaluator implements IntegerType{

	private long value;

	public long getValue() {
		return value;
	}

	public LongEvaluator(long value) {
		super();
		this.value = value;
	}

	public void evaluate() {
		stack[++top] = value;
	}

}
