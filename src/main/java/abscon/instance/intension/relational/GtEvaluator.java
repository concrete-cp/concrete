package abscon.instance.intension.relational;

public class GtEvaluator extends RelationalEvaluator {
public void evaluate() {
		top--;
		stack[top] = (stack[top+1] > stack[top] ? 1 : 0);
	}
}
