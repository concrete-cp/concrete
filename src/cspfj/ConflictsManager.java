/*
 * Created on 16 mars 2007
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package cspfj;

import java.util.Arrays;
import java.util.logging.Logger;

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

	//private final static Logger logger = Logger.getLogger("ConflictsManager");

	private boolean critic;

	public final boolean[] criticConstraints;

	public ConflictsManager(final Variable variable, final TieManager tieManager) {
		super();
		this.variable = variable;
		vid = variable.getId();
		this.domain = variable.getDomain();
		this.constraints = variable.getInvolvingConstraints();
		this.tieManager = tieManager;
		nbConflicts = new int[variable.getDomain().length];

		check = new boolean[constraints.length][domain.length];
		criticConstraints = new boolean[constraints.length];
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

				variable.assignBoolean(i);

				int indexConflicts = 0;

				for (int c = constraints.length; --c >= 0;) {
					final Constraint constraint = constraints[c];

					if (constraint.getArity() <= Constraint.MAX_ARITY
							&& !constraint.findValidTuple(variable
									.getPositionInConstraint(c), i)) {
						// logger.warning("No tuple found");
						indexConflicts += constraint.getWeight();
					}

				}

				tieManager.newValue(i, indexConflicts);

			}

			// variable.resetAssign();

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
			final boolean[] check = this.check[c];
			final Constraint constraint = constraints[c];
			final int position = constraint.getPosition(variable);
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
		initCritic() ;
	}
	
	private final void initCritic() {
		critic = false;
		final boolean[] criticConstraints = this.criticConstraints;
		for (int c = constraints.length; --c >= 0;) {
			if (criticConstraints[c] = critic(c)) {
				critic = true;
				assert currentConflicts > 0;
				break;
			}
		}
	}

	private boolean critic(int constraintPos) {
		final boolean[] check = this.check[constraintPos];
		final boolean currentCheck = check[assignedIndex];
		if (currentCheck) {
			return false;
		}
		for (int i = domain.length; --i >= 0;) {
			if (check[i]) {
				assert currentConflicts > 0;
				return true;
			}
		}
		return false;
	}

	public int getCurrentConflicts() {
		return currentConflicts;
	}

	public void update(final Constraint constraint, final int variablePos) {
		final TieManager tieManager = this.tieManager;
		tieManager.clear();
		final int[] nbConflicts = this.nbConflicts;
		final Variable variable = this.variable;

		// final int position = constraint.getPosition(variable);

		final int constraintPos = constraint.getPositionInVariable(variablePos);

		assert variable.getInvolvingConstraints()[constraintPos] == constraint;

		for (int i = domain.length; --i >= 0;) {
			if (variable.getRemovedLevel(i) >= 0) {
				continue;
			}

			final boolean check = constraint.checkFirstWith(variablePos, i);
			if (check ^ this.check[constraintPos][i]) {
				if (check) {
					nbConflicts[i] -= constraint.getWeight();
				} else {
					nbConflicts[i] += constraint.getWeight();
				}
				this.check[constraintPos][i] ^= true;
			}

			tieManager.newValue(i, nbConflicts[i]);
		}
		bestIndex = tieManager.getBestValue();
		currentConflicts = nbConflicts[assignedIndex];

//		updateCritic(constraintPos);
		initCritic() ;
	}

	private void updateCritic(int constraintPos) {
		final boolean wasCritic = criticConstraints[constraintPos];
		assert wasCritic ? critic : true;
		criticConstraints[constraintPos] = critic(constraintPos) ;
		if (criticConstraints[constraintPos]) {
			critic = true;
		} else if (wasCritic) {
			critic = false;
			for (int c = criticConstraints.length; --c >= 0;) {
				if (criticConstraints[c]) {
					critic = true;
					break;
				}
			}
		}

	}

	public boolean updateAfterIncrement(final Constraint constraint,
			final int pos) {
		
		final TieManager tieManager = this.tieManager;
		tieManager.clear();

		final int[] nbConflicts = this.nbConflicts;
		final Variable variable = this.variable;
		assert constraint.getPosition(variable) == pos;
		assert variable.getInvolvingConstraints()[constraint
				.getPositionInVariable(pos)] == constraint;

		final boolean[] check = this.check[constraint
				.getPositionInVariable(pos)];
		assert !check[assignedIndex];
		for (int i = domain.length; --i >= 0;) {
			if (variable.getRemovedLevel(i) >= 0) {
				continue;
			}
			if (!check[i]) {
				nbConflicts[i]++;
			}
			tieManager.newValue(i, nbConflicts[i]);
		}
		currentConflicts = nbConflicts[assignedIndex];
		if (bestIndex == tieManager.getBestValue()) {
			return false;
		}
		bestIndex = tieManager.getBestValue();
		return true;
	}

	public int getBestIndex(final TabuManager tabuManager,
			final int aspiration, final int nbIt) {
		final int limit = currentConflicts - aspiration;
		final int vid = this.vid;

		if (!tabuManager.isTabu(vid, bestIndex, nbIt)
				|| nbConflicts[bestIndex] < limit) {
			return bestIndex;
		}

		final TieManager tieManager = this.tieManager;
		tieManager.clear();

		final int[] nbConflicts = this.nbConflicts;
		final Variable variable = this.variable;

		for (int i = domain.length; --i >= 0;) {
			if (variable.getRemovedLevel(i) < 0
					&& (!tabuManager.isTabu(vid, i, nbIt) || nbConflicts[i] < limit)) {
				tieManager.newValue(i, nbConflicts[i]);
			}
		}

		return tieManager.getBestValue();
	}

	public void reAssign(final int index) {
		variable.reAssign(index);
		assignedIndex = index;
		currentConflicts = nbConflicts[index];
		initCritic() ;
	}

	public int getAssignedIndex() {
		return assignedIndex;
	}

	public boolean isCritic() {
		return critic;
	}

	public void shuffleBest() {
		final TieManager tieManager = this.tieManager;
		tieManager.clear() ;
		for (int i = domain.length; --i >= 0;) {
			if (variable.getRemovedLevel(i) < 0) {
				tieManager.newValue(i, nbConflicts[i]);
			}
		}

		bestIndex = tieManager.getBestValue();
	}
}
