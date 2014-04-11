package abscon.instance.components;


public class PExtensionConstraint extends PConstraint {

	private PRelation relation;

	public PRelation getRelation() {
		return relation;
	}

	public PExtensionConstraint(String name, PVariable[] scope, PRelation relation) {
		super(name, scope);
		this.relation = relation;
	}

	public int getMaximalCost() {
		return relation.getMaximalCost();
	}
	
	public long computeCostOf(int[] tuple) {
		return relation.computeCostOf(tuple);
	}

	public String toString() {
		return super.toString() + ", and associated relation " + relation.getName();
	}
}
