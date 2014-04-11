package abscon.instance.intension.arithmetic;

public final class AbsEvaluator extends Arity1ArithmeticEvaluator {
	public void evaluate() {
		stack[top] = Math.abs(stack[top]);
	}
}
