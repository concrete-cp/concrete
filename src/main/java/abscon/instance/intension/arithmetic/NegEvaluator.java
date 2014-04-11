package abscon.instance.intension.arithmetic;

public class NegEvaluator extends Arity1ArithmeticEvaluator {
	public void evaluate() {
		stack[top] = -stack[top];
	}
}
