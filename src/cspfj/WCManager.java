/*
 * Created on 16 mars 2007
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package cspfj;

import cspfj.constraint.Constraint;
import cspfj.problem.Variable;
import cspfj.util.TieManager;

public final class WCManager {

	private final Variable variable;

	private int bestIndex;

	private final TieManager tieManager;

	private int[] nbConflicts;

	private final int[] domain;

	private final Constraint[] constraints;

	private final int[] position;

	private final boolean[][] check;

	public WCManager(final Variable variable, final TieManager tieManager) {
		super();
		this.variable = variable;
		this.domain = variable.getDomain();
		this.constraints = variable.getInvolvingConstraints();
		this.tieManager = tieManager;
		nbConflicts = new int[variable.getDomain().length];
		position = new int[constraints.length];
		for (int i = position.length; --i >= 0;) {
			position[i] = constraints[i].getPosition(variable);
		}
		check = new boolean[constraints.length][domain.length];
	}

	public Variable getVariable() {
		return variable;
	}

	public int getBestIndex() {
		return bestIndex;
	}

	public int getBestImprovment() {
		return getImprovment(bestIndex);
	}

	public int getImprovment(final int index) {
		return nbConflicts[index] - nbConflicts[variable.getFirst()];
	}

//	private int nbConflicts(final int index) {
//		assert index < domain.length : index + " >= " + domain.length;
//		final Constraint[] constraints = this.constraints;
//		final int[] position = this.position;
//		int conflicts = 0;
//		for (int i = constraints.length; --i >= 0;) {
//			if (!constraints[i].checkFirstWith(position[i], index)) {
//				conflicts += constraints[i].getWeight();
//			}
//		}
//		return conflicts;
//	}

	private int getConflicts(final int index) {
		return nbConflicts[index];
	}

	public int getBestInitialIndex() {
		if (variable.isAssigned()) {
			return variable.getFirst();
		}

		final Constraint[] constraints = this.constraints;

		tieManager.clear();

		final Variable variable = this.variable;

		for (int i = domain.length; --i >= 0;) {
			if (variable.getRemovedLevel(i) >= 0) {
				continue;
			}

			variable.firstAssign(i);

			int indexConflicts = 0;

			for (Constraint c : constraints) {
				if (c.getArity() > Constraint.MAX_ARITY) {
					continue;
				}

				if (!c.findValidTuple(variable, i)) {
					indexConflicts += c.getWeight();
				}

			}

			tieManager.newValue(i, indexConflicts);

		}

		variable.resetAssign();

		bestIndex = tieManager.getBestValue();
		return bestIndex;
	}

	public void initNbConflicts() {
		for (int c = constraints.length; --c >= 0;) {
			final Constraint constraint = constraints[c];
			final int position = constraint.getPosition(variable);
			for (int i = domain.length; --i >= 0;) {
				if (variable.getRemovedLevel(i) >= 0) {
					continue;
				}

				check[c][i] = constraint.checkFirstWith(position, i);
				if (!check[c][i]) {
					nbConflicts[i] += constraint.getWeight();
				}
			}
		}
	}

	public int getCurrentConflicts() {
		return getConflicts(variable.getFirst());
	}

	public void update(Constraint constraint) {
		final TieManager tieManager = this.tieManager;
		tieManager.clear();
		final int[] nbConflicts = this.nbConflicts;
		final Variable variable = this.variable;

		final int c = constraint.getPositionInVariable(variable);
		final int position = constraint.getPosition(variable);
		for (int i = domain.length; --i >= 0;) {
			if (variable.getRemovedLevel(i) >= 0) {
				continue;
			}

			final boolean check = constraint.checkFirstWith(position, i);
			if (check != this.check[c][i]) {
				if (check) {
					nbConflicts[i] -= constraint.getWeight();
				} else {
					nbConflicts[i] += constraint.getWeight();
				}
				this.check[c][i] ^= true;
			}

			tieManager.newValue(i, nbConflicts[i]);
		}
		bestIndex = tieManager.getBestValue();
	}

	public void updateAfterIncrement(final Constraint constraint) {
		final TieManager tieManager = this.tieManager;
		tieManager.clear();

		final int[] nbConflicts = this.nbConflicts;
		final Variable variable = this.variable;

		final int c = constraint.getPositionInVariable(variable);

		for (int i = domain.length; --i >= 0;) {
			if (variable.getRemovedLevel(i) >= 0) {
				continue;
			}
			if (!check[c][i]) {
				nbConflicts[i]++;
			}
			tieManager.newValue(i, nbConflicts[i]);
		}
		bestIndex = tieManager.getBestValue();
	}

	// /**
	// * @param index
	// * La valeur à assigner
	// */
	// public void reAssign(final int index, final TieManager tieManager) {
	// variable.reAssign(index);
	// for (Constraint c : variable.getInvolvingConstraints()) {
	// for (Variable n : c.getInvolvedVariables()) {
	// if (n != variable) {
	// n.updateConflicts(c, c.getPosition(n));
	// n.updateBestIndex(tieManager);
	// }
	// }
	// }
	// }

}
