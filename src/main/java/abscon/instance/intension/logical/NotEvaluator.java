package abscon.instance.intension.logical;

public class NotEvaluator extends Arity1LogicalEvaluator {
	public void evaluate() {
		stack[top] = 1 - stack[top]; //(stack[nbStackElements - 1] == 1 ? 0 : 1);
	}
}