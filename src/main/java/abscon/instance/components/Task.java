package abscon.instance.components;

import abscon.instance.Toolkit;

public class Task {
	private Object origin; // may be null if absent, an Integer or a Variable

	private int originPositionInScope;

	private int originValue;

	private Object duration;

	private int durationPositionInScope;

	private int durationValue;

	private Object end;

	private int endPositionInScope;

	private int endValue;

	private Object height;

	private int heightPositionInScope;

	private int heightValue;

	public Object getOrigin() {
		return origin;
	}

	public int getOriginValue() {
		return originValue;
	}

	public Object getDuration() {
		return duration;
	}

	public int getDurationValue() {
		return durationValue;
	}

	public Object getEnd() {
		return end;
	}

	public int getEndValue() {
		return endValue;
	}

	public Object getHeight() {
		return height;
	}

	public int getHeightValue() {
		return heightValue;
	}

	public Task(Object origin, Object duration, Object end, Object height) {
		this.origin = origin;
		this.duration = duration;
		this.end = end;
		this.height = height;
	}

	public void setVariablePositions(Object[] scope) {
		this.originPositionInScope = Toolkit.searchFirstObjectOccurrenceIn(origin, scope);
		this.durationPositionInScope = Toolkit.searchFirstObjectOccurrenceIn(duration, scope);
		this.endPositionInScope = Toolkit.searchFirstObjectOccurrenceIn(end, scope);
		this.heightPositionInScope = Toolkit.searchFirstObjectOccurrenceIn(height, scope);
	}

	public int evaluate(int[] tuple) {
		if (origin != null)
			originValue = origin instanceof Integer ? (Integer) origin : tuple[originPositionInScope];
		if (duration != null)
			durationValue = duration instanceof Integer ? (Integer) duration : tuple[durationPositionInScope];
		if (end != null)
			endValue = end instanceof Integer ? (Integer) end : tuple[endPositionInScope];
		if (origin != null && duration != null && end != null && originValue + durationValue != endValue)
			return 1;
		if (origin == null)
			originValue = endValue - durationValue;
		if (end == null)
			endValue = originValue + durationValue;
		heightValue = height instanceof Integer ? (Integer) height : tuple[heightPositionInScope];
		return 0;
	}
	
	
	
	

//	public int getEarliestStartingTime() {
//		if (origin != null) 
//			return origin instanceof Integer ? ((Integer)origin).intValue() : ((Variable)origin).getDomain().getFirstValidValue();
//		int earliestFinishTime = 	end instanceof Integer ? ((Integer)end).intValue() : ((Variable)end).getDomain().getFirstValidValue();
//		int longestDuration = 	duration instanceof Integer ? ((Integer)duration).intValue() : ((Variable)duration).getDomain().getLastValidValue();
//		return 	earliestFinishTime - longestDuration;
//	}
//	
//	
//	public int getEarliestFinishTime() {
//		if (end != null) 
//			return end instanceof Integer ? ((Integer)end).intValue() : ((Variable)end).getDomain().getFirstValidValue();
//		int earliestStartingTime = 	origin instanceof Integer ? ((Integer)origin).intValue() : ((Variable)origin).getDomain().getFirstValidValue();
//		int shortestDuration = 	duration instanceof Integer ? ((Integer)duration).intValue() : ((Variable)duration).getDomain().getFirstValidValue();
//		return 	earliestStartingTime + shortestDuration;
//	}
	
	
	
	
	 public void displayEvaluations() {
	 System.out.println(originValue + " " + durationValue + " " + endValue + " " + heightValue);
	 }

	public String toString() {
		return " [origin=" + origin + " duration=" + duration + " end=" + end + " height=" + height + "]\n\t";
	}
}