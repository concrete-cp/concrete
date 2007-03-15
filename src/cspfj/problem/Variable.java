/**
 * CSPFJ - CSP solving API for Java
 * Copyright (C) 2006 Julien VION
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
 */

package cspfj.problem;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Random;
import java.util.Set;
import java.util.TreeSet;

import cspfj.constraint.Constraint;
import cspfj.util.BooleanArray;
import cspfj.util.OrderedChain;
import cspfj.util.TieManager;
import cspfj.util.UnOrderedChain;

/**
 * @author scand1sk
 * 
 */
public final class Variable implements Comparable<Variable>, Cloneable {
	/**
	 * Contraintes impliquant la variable.
	 */
	private Constraint constraints[];

	/**
	 * Liste des variables voisines.
	 */
	private Variable[] neighbours;

	/**
	 * Domaine de la variable (ensemble de valeurs).
	 */
	private final int[] domain;

	/**
	 * Éléments supprimés du domaine (-1 : présent, n : élément supprimé au
	 * niveau n)
	 */
	private int[] removed;

	private OrderedChain chain;

	private UnOrderedChain absents;

	private int[] booleanDomain;

	private int[] beforeAssignDomain;

	private boolean[][] conflicts;

	private int[] nbConflicts;

	/**
	 * Taille actuelle du domaine.
	 */
	private int domainSize;

	/**
	 * Variable assignée ?
	 */
	private int assignedIndex;

	private boolean assigned;

	/**
	 * ID de la variable.
	 */
	private final int id;

	/**
	 * Pour générer l'ID.
	 */
	private static int nbV = 0;

	private final String name;

	// private static final Logger logger = Logger.getLogger("cspfj.Variable");

	public Variable(final int[] dom) {
		this(dom, null);
	}

	/**
	 * @param dom
	 *            Liste des éléments du domaine.
	 * @param p
	 *            Le problème lié.
	 */
	public Variable(final int[] dom, String name) {
		this.domain = dom.clone();
		domainSize = domain.length;
		this.removed = new int[domain.length];
		Arrays.fill(removed, -1);

		booleanDomain = BooleanArray.newBooleanArray(domainSize, true);
		beforeAssignDomain = booleanDomain.clone();
		this.assignedIndex = -1;
		assigned = false;
		id = nbV++;

		this.name = name;

		chain = new OrderedChain(domain.length);

		final int[] order = new int[domain.length];
		for (int i = domain.length; --i >= 0;) {
			order[i] = i;
		}
		chain.reOrder(order, removed);

		absents = new UnOrderedChain(domain.length);

		nbConflicts = new int[domain.length];
	}

	/**
	 * Réinitialise le générateur d'ID (pour charger un nouveau problème).
	 */
	public static void resetVId() {
		nbV = 0;
	}

	@Override
	public String toString() {
		if (name == null) {
			return "X" + id;
		}
		return name;

	}

	public void reOrder(final int[] order) {
		chain.reOrder(order, removed);
	}

	/**
	 * @param constraints
	 *            Liste des contraintes impliquant la variable
	 */
	public void setInvolvingConstraints(final Constraint[] constraints) {
		this.constraints = constraints;

		final Set<Variable> neighb = new TreeSet<Variable>();

		for (Constraint c : constraints) {
			for (Variable v : c.getInvolvedVariables()) {
				if (v == this) {
					continue;
				}
				neighb.add(v);
			}
		}

		this.neighbours = neighb.toArray(new Variable[neighb.size()]);

		for (int i = constraints.length; --i >= 0;) {
			constraints[i].setPositionInVariable(this, i);
		}

		conflicts = new boolean[constraints.length][domain.length];
	}

	/**
	 * @return La taille du domaine
	 */
	public int getDomainSize() {
		return assigned ? 1 : domainSize;
	}

	/**
	 * @param value
	 *            La valeur dont on veut obtenir l'index
	 * @return L'index de la valeur donnée en paramètre
	 * @throws NotInDomainException
	 */
	public int index(final int value) {
		final int[] domain = this.domain;
		for (int i = domain.length; --i >= 0;) {
			if (domain[i] == value) {
				return i;
			}
		}

		return -1;
	}

	/**
	 * @return L'état (assignée ou non) de la variable
	 */
	public boolean isAssigned() {
		return assigned;
	}

	/**
	 * @return Liste des variables voisines.
	 */
	public Variable[] getNeighbours() {
		return neighbours;
	}

	/**
	 * @param index
	 *            L'index à tester
	 * @return True si l'index est absent
	 */
	public boolean isPresent(final int index) {
		return assigned ? assignedIndex == index : removed[index] < 0;
	}

	/**
	 * @param index
	 *            La valeur à assigner
	 */
	public void assign(final int index, final Problem problem) {
		assert !assigned;
		assert isPresent(index);

		assignedIndex = index;
		assigned = true;
		System.arraycopy(booleanDomain, 0, beforeAssignDomain, 0,
				booleanDomain.length);
		BooleanArray.setSingle(booleanDomain, index);
		assert !reinitBooleanDomain();
		problem.decreaseFutureVariables();

		assert checkIndexValidity(index);

	}

	/**
	 * @param index
	 *            La valeur à assigner
	 */
	public void reAssign(final int index, final TieManager tieManager) {
		assignedIndex = index;
		for (Constraint c : constraints) {
			for (Variable n : c.getInvolvedVariables()) {
				if (n != this) {
					n.updateConflicts(c, c.getPosition(n));
					n.updateBestIndex(tieManager);
				}
			}
		}
	}

	public void firstAssign(final int index) {
		assignedIndex = index;
		assigned = true;
	}

	public void resetAssign() {
		assigned = false;
	}

	public boolean checkIndexValidity(final int index) {
		for (Constraint c : constraints) {
			if (!c.findValidTuple(this, index)) {
				return false;
			}
		}
		return true;
	}

	/**
	 * @param level
	 *            Le niveau en cours
	 */
	public void unassign(final Problem problem) {
		assert assigned;
		assigned = false;
		problem.increaseFutureVariables();
		System.arraycopy(beforeAssignDomain, 0, booleanDomain, 0,
				booleanDomain.length);
	}

	/**
	 * @param level
	 *            Le niveau à restaurer
	 * @return True si au moins une valeur a été restaurée
	 */
	public void restoreLevel(final int level) {
		assert !assigned;

		final int[] removed = this.removed;
		final UnOrderedChain absents = this.absents;

		for (int i = absents.getLast(); i >= 0; i = absents.getPrev(i)) {
			if (removed[i] >= level) {
				restore(i);
			}
		}
	}

	private void restore(final int index) {
		assert !isPresent(index);

		final int[] removed = this.removed;

		domainSize++;
		removed[index] = -1;
		BooleanArray.set(booleanDomain, index, true);

		chain.restore(index, removed);
		absents.remove(index);
	}

	/**
	 * @param index
	 *            L'index à supprimer
	 * @param level
	 *            Le niveau en cours
	 */
	public void remove(final int index, final int level) {
		assert !assigned;
		assert isPresent(index);
		removed[index] = level;
		BooleanArray.set(booleanDomain, index, false);

		domainSize--;

		chain.remove(index);
		absents.add(index);
	}

	/**
	 * @return L'ID de la variable
	 */
	public int getId() {
		return id;
	}

	public int getNbSupports(final int index) {
		int nbSupports = 0;
		for (Constraint c : constraints) {
			nbSupports += c.getNbSupports(this, index);
		}
		return nbSupports;
	}

	public Constraint[] getInvolvingConstraints() {
		return constraints;
	}

	public int[] getDomain() {
		return domain;
	}

	public int getFirst() {
		if (assigned) {
			return assignedIndex;
		}
		assert isPresent(chain.getFirst());
		return chain.getFirst();

	}

	public int getLast() {
		return assigned ? assignedIndex : chain.getLast();
	}

	public double getWDeg() {
		double count = 0;
		if (constraints == null) {
			return 1;
		}

		for (Constraint c : constraints) {
			count += c.getFreedomDegree() * c.getWeight();
		}
		return count;
	}

	public double getDDeg() {
		double count = 0;
		if (constraints == null) {
			return 1;
		}

		for (Constraint c : constraints) {
			count += c.getFreedomDegree();
		}
		return count;
	}

	public int compareTo(final Variable arg0) {
		final int compare = domainSize - arg0.getDomainSize();

		if (compare == 0) {
			final double compareDeg = arg0.getWDeg() - getWDeg();
			if (compareDeg == 0) {
				return id - arg0.getId();
			}
			return compareDeg > 0 ? 1 : -1;
		}
		return compare;
	}

	public boolean equals(final Variable variable) {
		return variable.getId() == id;
	}

	public void makeSingleton(final int value, final int level) {
		final int[] domain = this.domain;
		for (int i = getFirst(); i >= 0; i = getNext(i)) {
			if (domain[i] != value) {
				remove(i, level);
			}
		}
	}

	public void makeSingletonIndex(final int sol, final int level) {
		for (int i = getFirst(); i >= 0; i = getNext(i)) {
			if (i != sol) {
				remove(i, level);
			}
		}

	}

	public void empty(final int level) {

		if (getDomainSize() > 0) {
			for (int i = getFirst(); i >= 0; i = getNext(i)) {

				remove(i, level);

			}
		}

	}

	public Integer[] getCurrentDomain() {
		final List<Integer> values = new ArrayList<Integer>();

		for (int i = getFirst(); i >= 0; i = getNext(i)) {
			values.add(domain[i]);
		}

		return values.toArray(new Integer[values.size()]);
	}

	public String getName() {
		return name;
	}

	public int getRandomPresentIndex(final Random random) {
		int index;
		do {
			index = random.nextInt(domain.length);
		} while (removed[index] >= 0);

		return index;
	}

	private int bestIndex;

	public int bestImprovment() {
		return bestIndex;
	}

	public int getImprovment(final int index) {
		return getConflicts(index) - getConflicts(assignedIndex);
	}

	private int nbConflicts(final int index) {
		assert index < domain.length : index + " >= " + domain.length;
		int conflicts = 0;
		for (int i = constraints.length; --i >= 0;) {
			if (this.conflicts[i][index]) {
				conflicts += constraints[i].getWeight();
			}
		}
		return conflicts;
	}

	private int getConflicts(final int index) {
		return nbConflicts[index];
	}

	public int getBestInitialIndex(final TieManager tieManager) {
		if (assigned) {
			return assignedIndex;
		}

		final int[] removed = this.removed;
		final Constraint[] constraints = this.constraints;

		tieManager.clear();
		assigned = true;
		for (int i = domain.length; --i >= 0;) {
			if (removed[i] >= 0) {
				continue;
			}

			assignedIndex = i;

			int indexConflicts = 0;

			for (Constraint c : constraints) {
				if (c.getArity() > Constraint.MAX_ARITY) {
					continue;
				}

				if (!c.findValidTuple(this, i)) {
					indexConflicts += c.getWeight();
				}

			}

			tieManager.newValue(i, indexConflicts);

		}

		assignedIndex = -1;
		assigned = false;

		bestIndex = tieManager.getBestValue();
		return bestIndex;
	}

	public void updateConflicts(final Constraint constraint, final int position) {

		final int oldIndex = assignedIndex;

		final int constraintPosition = constraint.getPositionInVariable(this);

		for (int i = domain.length; --i >= 0;) {
			assignedIndex = i;

			conflicts[constraintPosition][i] = constraint.checkFirstWith(
					position, i) ^ true;

		}

		assignedIndex = oldIndex;
	}

	public void initNbConflicts(final TieManager tieManager) {
		for (Constraint c : constraints) {
			updateConflicts(c, c.getPosition(this));
		}
		updateBestIndex(tieManager);
	}

	public int getCurrentConflicts() {
		return getConflicts(assignedIndex);
	}

	public int getRemovedLevel(final int index) {
		return removed[index];
	}

	public int getNext(final int index) {
		if (assigned) {
			if (index != assignedIndex) {
				return assignedIndex;
			}
			return -1;
		}

		return chain.getNext(index);
	}

	public int getLastAbsent() {
		return absents.getLast();
	}

	public int getPrevAbsent(final int index) {
		return absents.getPrev(index);
	}

	public int[] getBooleanDomain() {
		return booleanDomain;
	}

	public boolean reinitBooleanDomain() {
		boolean change = false;
		if (assigned) {
			for (int i = removed.length; --i >= 0;) {
				change |= BooleanArray
						.set(booleanDomain, i, i == assignedIndex);
			}
		} else {
			for (int i = removed.length; --i >= 0;) {
				change |= BooleanArray.set(booleanDomain, i, removed[i] < 0);
			}
		}
		return change;
	}

	public int getPrev(final int index) {
		return chain.getPrev(index);
	}

	public void updateBestIndex(final TieManager tieManager) {
		tieManager.clear();
		final int[] removed = this.removed;
		final int[] nbConflicts = this.nbConflicts;
		for (int i = domain.length; --i >= 0;) {
			if (removed[i] >= 0) {
				continue;
			}
			final int nbc = nbConflicts(i);
			nbConflicts[i] = nbc;

			tieManager.newValue(i, nbc);
		}
		bestIndex = tieManager.getBestValue();
	}

	public void updateBestIndexAfter(final Constraint constraint,
			final TieManager tieManager) {
		tieManager.clear();
		final int[] removed = this.removed;
		final int[] nbConflicts = this.nbConflicts;
		final boolean[] conflicts = this.conflicts[constraint
				.getPositionInVariable(this)];

		for (int i = domain.length; --i >= 0;) {
			if (removed[i] >= 0) {
				continue;
			}
			if (conflicts[i]) {
				nbConflicts[i]++;
			}
			tieManager.newValue(i, nbConflicts[i]);
			bestIndex = tieManager.getBestValue();
		}
	}

	public Variable clone() throws CloneNotSupportedException {
		final Variable variable = (Variable) super.clone();

		variable.removed = removed.clone();

		variable.booleanDomain = booleanDomain.clone();
		variable.beforeAssignDomain = booleanDomain.clone();

		variable.chain = chain.clone();

		variable.absents = absents.clone();

		variable.nbConflicts = nbConflicts.clone();

		return variable;
	}

}
