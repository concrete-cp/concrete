package abscon.instance.intension.arithmetic;

import abscon.instance.intension.types.AssociativeType;
import abscon.instance.intension.types.SymmetricType;

public class MaxEvaluator extends Arity2ArithmeticEvaluator implements SymmetricType, AssociativeType {
	public void evaluate() {
		top--;
		stack[top] = Math.max(stack[top + 1], stack[top]);
	}
}
