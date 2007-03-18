/*
 * Created on 16 mars 2007
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package cspfj;

import java.util.Arrays;

import cspfj.constraint.Constraint;
import cspfj.problem.Variable;
import cspfj.util.TieManager;

public final class WCManager {

	private final Variable variable;

	private final int vid;

	private int bestIndex;

	private final TieManager tieManager;

	private final int[] nbConflicts;

	private final int[] domain;

	private final Constraint[] constraints;

	private final int[] position;

	private final boolean[][] check;

	private int currentConflicts;

	private int assignedIndex;

	public WCManager(final Variable variable, final TieManager tieManager) {
		super();
		this.variable = variable;
		vid = variable.getId();
		this.domain = variable.getDomain();
		this.constraints = variable.getInvolvingConstraints();
		this.tieManager = tieManager;
		nbConflicts = new int[variable.getDomain().length];
		position = new int[constraints.length];
		for (int i = position.length; --i >= 0;) {
			position[i] = constraints[i].getPosition(variable);
		}
		check = new boolean[constraints.length][domain.length];
		assignedIndex = variable.getFirst();
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
		return nbConflicts[index] - currentConflicts;
	}

	// private int nbConflicts(final int index) {
	// assert index < domain.length : index + " >= " + domain.length;
	// final Constraint[] constraints = this.constraints;
	// final int[] position = this.position;
	// int conflicts = 0;
	// for (int i = constraints.length; --i >= 0;) {
	// if (!constraints[i].checkFirstWith(position[i], index)) {
	// conflicts += constraints[i].getWeight();
	// }
	// }
	// return conflicts;
	// }

	// private int getConflicts(final int index) {
	// return nbConflicts[index];
	// }

	public void assignBestInitialIndex() {
		if (variable.isAssigned()) {
			bestIndex = assignedIndex;
		} else {

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

			bestIndex = assignedIndex = tieManager.getBestValue();

			variable.firstAssign(bestIndex);
		}
		currentConflicts = nbConflicts[bestIndex];

	}

	public void initNbConflicts() {
		final int[] nbConflicts = this.nbConflicts;
		Arrays.fill(nbConflicts, 0);
		final Constraint[] constraints = this.constraints;
		final Variable variable = this.variable;
		final int domain = this.domain.length;
		for (int c = constraints.length; --c >= 0;) {
			final Constraint constraint = constraints[c];
			final int position = constraint.getPosition(variable);
			final boolean[] check = this.check[c];
			for (int i = domain; --i >= 0;) {
				if (variable.getRemovedLevel(i) >= 0) {
					continue;
				}

				check[i] = constraint.checkFirstWith(position, i);
				if (!check[i]) {
					nbConflicts[i] += constraint.getWeight();
				}
			}
		}
		currentConflicts = nbConflicts[assignedIndex];
	}

	public int getCurrentConflicts() {
		return currentConflicts;
	}

	public void update(final Constraint constraint) {
		final TieManager tieManager = this.tieManager;
		tieManager.clear();
		final int[] nbConflicts = this.nbConflicts;
		final Variable variable = this.variable;

		final int c = constraint.getPositionInVariable(variable);
		final int position = this.position[c];// constraint.getPosition(variable);
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
		currentConflicts = nbConflicts[assignedIndex];
	}

	public boolean updateAfterIncrement(final Constraint constraint, final int pos) {
		final TieManager tieManager = this.tieManager;
		tieManager.clear();

		final int[] nbConflicts = this.nbConflicts;
		final Variable variable = this.variable;

		final boolean[] check = this.check[constraint
				.getPositionInVariable(pos)];

		for (int i = domain.length; --i >= 0;) {
			if (variable.getRemovedLevel(i) >= 0) {
				continue;
			}
			if (!check[i]) {
				nbConflicts[i]++;
			}
			tieManager.newValue(i, nbConflicts[i]);
		}
		final boolean changed = (bestIndex==tieManager.getBestValue());
		bestIndex = tieManager.getBestValue();
		currentConflicts = nbConflicts[assignedIndex];
		return changed;
	}

	public int getBestIndex(final TabuManager tabuManager) {
		if (!tabuManager.isTabu(vid, bestIndex)) {
			return bestIndex;
		}

		final TieManager tieManager = this.tieManager;
		tieManager.clear();

		final int[] nbConflicts = this.nbConflicts;
		final Variable variable = this.variable;

		for (int i = domain.length; --i >= 0;) {
			if (variable.getRemovedLevel(i) >= 0) {
				continue;
			}
			if (!tabuManager.isTabu(vid, i)) {
				tieManager.newValue(i, nbConflicts[i]);
			}
		}

		return tieManager.getBestValue();
	}

	public void reAssign(final int index) {
		variable.reAssign(index);
		assignedIndex = index;
		currentConflicts = nbConflicts[index];
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
