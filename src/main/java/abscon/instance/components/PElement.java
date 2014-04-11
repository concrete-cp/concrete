package abscon.instance.components;

import abscon.instance.Toolkit;

public class PElement extends PGlobalConstraint {
	private PVariable index;

	private Object[] table;

	private Object value;
	
	private int offset=1; // by default, indexing starts from 1

	private int indexPositionInScope;

	private int[] tablePositionsInScope;

	private int valuePositionInScope;

	public PElement(String name, PVariable[] scope, PVariable indexVariable, Object[] table, Object value) {
		super(name, scope);
		this.index = indexVariable;
		this.table = table;
		this.value = value;
		indexPositionInScope = Toolkit.searchFirstObjectOccurrenceIn(indexVariable, scope);
		tablePositionsInScope = computeObjectPositionsInScope(table);
		valuePositionInScope = Toolkit.searchFirstObjectOccurrenceIn(value, scope);
	}

	public long computeCostOf(int[] tuple) {
		int indexInTable = tuple[indexPositionInScope]-offset;
		Object object = table[indexInTable];
		int result = (object instanceof Integer ? (Integer) object : tuple[tablePositionsInScope[indexInTable]]);
		boolean satisfied = result == (value instanceof Integer ? (Integer) value : tuple[valuePositionInScope]);
		return satisfied ? 0 : 1;
	}

	public String toString() {
		String s = super.toString() + " : element\n\t";
		s += "index=" + index.getName() + "  table=";
		for (int i = 0; i < table.length; i++)
			s += computeStringRepresentationOf(table[i]) + " ";
		s += "  value=" + computeStringRepresentationOf(value);
		return s;
	}
}
