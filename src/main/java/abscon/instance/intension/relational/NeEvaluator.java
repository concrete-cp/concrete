package abscon.instance.intension.relational;

import abscon.instance.intension.types.AssociativeType;
import abscon.instance.intension.types.SymmetricType;

public class NeEvaluator extends RelationalEvaluator implements SymmetricType, AssociativeType {
	public void evaluate() {
		top--;
		stack[top] = (stack[top + 1] != stack[top] ? 1 : 0);
	}
}
