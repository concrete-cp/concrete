package cspfj.problem;

import java.util.ArrayList;
import java.util.List;

import cspfj.constraint.Constraint;
import cspfj.constraint.DynamicConstraint;
import cspfj.priorityqueues.Identified;
import cspom.variable.CSPOMVariable;

public abstract class AbstractVariable {

	private static int nbV = 0;
	private final int id;
	private int[] positionInConstraint;
	private final CSPOMVariable cspomVariable;
	private Constraint[] constraints;

	private DynamicConstraint[] dynamicConstraints;

	private boolean assigned;

	/**
	 * Reinit ID generator (before loading a new problem).
	 */
	public static void resetVId() {
		nbV = 0;
	}

	public AbstractVariable(final CSPOMVariable cspomVariable) {
		super();
		this.cspomVariable = cspomVariable;
		assigned = false;
		id = nbV++;
	}

	/**
	 * @param constraints2
	 *            Liste des contraintes impliquant la variable
	 */
	public void setInvolvingConstraints(final Constraint[] constraints2) {
		this.constraints = constraints2;
		final List<DynamicConstraint> dynamicConstraints = new ArrayList<DynamicConstraint>();
		for (Constraint c : constraints2) {
			if (c instanceof DynamicConstraint) {
				dynamicConstraints.add((DynamicConstraint) c);
			}
		}

		this.dynamicConstraints = dynamicConstraints
				.toArray(new DynamicConstraint[dynamicConstraints.size()]);

		positionInConstraint = new int[constraints2.length];
		for (int i = constraints2.length; --i >= 0;) {
			updatePositionInConstraint(i);
		}

	}

	public void updatePositionInConstraint(final int constraintPosition) {
		positionInConstraint[constraintPosition] = constraints[constraintPosition]
				.getPosition(this);
	}

	public boolean isAssigned() {
		return assigned;
	}

	public void assign(final int index, final Problem problem) {
		assert !assigned;
		assert domain.present(index);

		assigned = true;
		domain.setSingle(index);

		problem.decreaseFutureVariables();
	}

	public void unassign(final Problem problem) {
		assert assigned;
		assigned = false;
		problem.increaseFutureVariables();
	}

	public int getId() {
		return id;
	}

	public Constraint[] getInvolvingConstraints() {
		return constraints;
	}

	public DynamicConstraint[] getDynamicConstraints() {
		return dynamicConstraints;
	}

	public int getPositionInConstraint(final int constraint) {
		return positionInConstraint[constraint];
	}

	public void setLevel(int level) {
		domain.setLevel(level);
	}

	public void restoreLevel(int level) {
		domain.restoreLevel(level);
	}

	public void reset(Problem problem) {
		if (isAssigned()) {
			unassign(problem);
		}
		domain.restoreLevel(0);
	}

	public CSPOMVariable getCspomVariable() {
		return cspomVariable;
	}
	
	public abstract int getDomainSize() ;

}