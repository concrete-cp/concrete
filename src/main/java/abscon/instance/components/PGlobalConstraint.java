package abscon.instance.components;

import abscon.instance.Toolkit;

public abstract class PGlobalConstraint extends PConstraint {
	public PGlobalConstraint(String name, PVariable[] scope) {
		super(name, scope);
	}

	protected int[] computeObjectPositionsInScope(Object[] objects) {
		int[] t = new int[objects.length];
		for (int i = 0; i < objects.length; i++)
			t[i] = Toolkit.searchFirstObjectOccurrenceIn(objects[i], scope);
		return t;
	}

	// object is either an Integer, a PVariable or null
	protected String computeStringRepresentationOf(Object object) {
		if (object == null)
			return "nil";
		if (object instanceof PVariable)
			return ((PVariable) object).getName();
		return object.toString();
	}

}
