package abscon.instance.intension.arithmetic;

public class PowEvaluator extends Arity2ArithmeticEvaluator {
	public void evaluate() {
		top--;
		stack[top] = (long) Math.pow(stack[top+1], stack[top]);
	}
}