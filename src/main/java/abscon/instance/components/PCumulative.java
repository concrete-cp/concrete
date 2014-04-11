package abscon.instance.components;

public class PCumulative extends PGlobalConstraint {
	private Task[] tasks;

	private int limit;

	public PCumulative(String name, PVariable[] scope, Task[] tasks, int limit) {
		super(name, scope);
		this.tasks = tasks;
		for (Task task : tasks) 
			task.setVariablePositions(scope); 
		this.limit = limit;
	}

	public long computeCostOf(int[] tuple) {
		for (Task task : tasks) {
			if (task.evaluate(tuple) == 1)
				return 1;
			// task.displayEvaluations();
		}
		for (int i = 0; i < tasks.length; i++) {
			for (int period = tasks[i].getOriginValue(); period < tasks[i].getEndValue(); period++) {
				int heightSum = tasks[i].getHeightValue();
				for (int j = i + 1; j < tasks.length; j++) {
					if (period >= tasks[j].getOriginValue() && period < tasks[j].getEndValue())
						heightSum += tasks[j].getHeightValue();
				}
				if (heightSum > limit) {
					// System.out.println(" i = " + i + " time = " + period);
					return 1;
				}
			}
		}
		return 0;
	}

	public String toString() {
		String s = super.toString() + " : cumulative\n\t";
		for (int i = 0; i < tasks.length; i++) {
			s += "  [origin=" + computeStringRepresentationOf(tasks[i].getOrigin()) + " " + "duration=" + computeStringRepresentationOf(tasks[i].getDuration()) + " ";
			s += "end=" + computeStringRepresentationOf(tasks[i].getEnd()) + " " + "height=" + computeStringRepresentationOf(tasks[i].getHeight()) + "]\n\t";
		}
		s += "nbTasks=" + tasks.length + " limit=" + limit;
		return s;
	}
}
