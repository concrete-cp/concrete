package abscon.instance.intension.arithmetic;

public class IfEvaluator extends Arity3ArithmeticEvaluator {
	public void evaluate() { 
		top-=2;
		if (stack[top+2] == 1)
			stack[top]=stack[top+1];
	}
}
