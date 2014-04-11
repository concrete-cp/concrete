package abscon.instance.intension.arithmetic;

public class ModEvaluator extends Arity2ArithmeticEvaluator {
	public void evaluate() {
		top--;
		stack[top] = stack[top+1] % stack[top];
	}
}
