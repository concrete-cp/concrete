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

public final class ConflictsManager {

	private final Variable variable;

	private final int vid;

	private int bestIndex;

	private final TieManager tieManager;

	private final int[] nbConflicts;

	private final int[] domain;

	private final Constraint[] constraints;

	private final boolean[][] check;

	private int currentConflicts;

	private int assignedIndex;

	public ConflictsManager(final Variable variable, final TieManager tieManager) {
		super();
		this.variable = variable;
		vid = variable.getId();
		this.domain = variable.getDomain();
		this.constraints = variable.getInvolvingConstraints();
		this.tieManager = tieManager;
		nbConflicts = new int[variable.getDomain().length];

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

	public void assignBestInitialIndex() {
		if (variable.isAssigned()) {
			bestIndex = assignedIndex;
		} else {
			final TieManager tieManager = this.tieManager;
			final Constraint[] constraints = this.constraints;

			tieManager.clear();

			final Variable variable = this.variable;

			for (int i = domain.length; --i >= 0;) {
				if (variable.getRemovedLevel(i) >= 0) {
					continue;
				}

				variable.firstAssign(i);

				int indexConflicts = 0;

				for (int c = constraints.length; --c >= 0;) {
					final Constraint constraint = constraints[c];
					if (constraint.getArity() > Constraint.MAX_ARITY) {
						continue;
					}

					if (!constraint.findValidTuple(variable
							.getPositionInConstraint(c), i)) {
						indexConflicts += constraint.getWeight();
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

		final int position = constraint.getPosition(variable);

		final int c = constraint.getPositionInVariable(position);

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

	public boolean updateAfterIncrement(final Constraint constraint,
			final int pos) {
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
		final boolean changed = (bestIndex == tieManager.getBestValue());
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

}
