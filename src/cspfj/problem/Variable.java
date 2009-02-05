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
import java.util.Collection;
import java.util.List;

import cspfj.constraint.Constraint;
import cspfj.constraint.DynamicConstraint;
import cspfj.util.BooleanArray;
import cspfj.util.OrderedChain;
import cspfj.util.UnOrderedChain;

/**
 * @author scand1sk
 * 
 */
public final class Variable implements Cloneable {
	/**
	 * Contraintes impliquant la variable.
	 */
	private Constraint[] constraints;

	private DynamicConstraint[] dynamicConstraints;

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
	 * Pour générer l'ID.
	 */
	private static int nbV = 0;

	/**
	 * ID de la variable.
	 */
	private final int id;

	private final String name;

	private int[] positionInConstraint;

	// private int[] weights;

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

		if (name == null) {
			this.name = "X" + id;
		} else {
			this.name = name;
		}

		chain = new OrderedChain(domain.length);

		final Integer[] order = new Integer[domain.length];
		for (int i = domain.length; --i >= 0;) {
			order[i] = i;
		}
		chain.reOrder(order, removed);

		absents = new UnOrderedChain(domain.length);
	}

	// public void attachWeights(int[] weights) {
	// this.weights = weights;
	// }

	/**
	 * Réinitialise le générateur d'ID (pour charger un nouveau problème).
	 */
	public static void resetVId() {
		nbV = 0;
	}

	@Override
	public String toString() {
		return name + "[" + getDomainSize() + "]";
	}

	public void reOrder(final Integer[] order) {
		chain.reOrder(order, removed);
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
		constraints[constraintPosition].setPositionInVariable(
				positionInConstraint[constraintPosition], constraintPosition);
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
	 * @param index
	 *            L'index à tester
	 * @return True si l'index est absent
	 */
	public boolean isPresent(final int index) {
		return assigned ? assignedIndex == index : removed[index] < 0;
	}

	private void assgn(final int index, final Problem problem) {
		assert !assigned;
		assert isPresent(index);

		assignedIndex = index;
		assigned = true;
		System.arraycopy(booleanDomain, 0, beforeAssignDomain, 0,
				booleanDomain.length);
		BooleanArray.setSingle(booleanDomain, index);
		assert !reinitBooleanDomain();
		problem.decreaseFutureVariables();

	}

	/**
	 * @param index
	 *            La valeur à assigner
	 */
	public void assign(final int index, final Problem problem) {
		assgn(index, problem);
		// assert checkIndexValidity(index) : "Tried to assign an invalid
		// index";

	}

	// public boolean assignNotAC(final int index, final Problem problem) {
	// assgn(index, problem);
	// return checkIndexValidity(index);
	// }

	public void firstAssign(final int index) {
		assignedIndex = index;
		assigned = true;
	}

	public void resetAssign() {
		assigned = false;
	}

	// private boolean checkIndexValidity(final int index) {
	// for (int c = constraints.length; --c >= 0;) {
	// if (!constraints[c].findValidTuple(positionInConstraint[c], index)) {
	// return false;
	// }
	// }
	// return true;
	// }

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

		// if (weights != null) {
		// weights[index]++;
		// }
	}

	// public int getWeight(final int index) {
	// return weights[index];
	// }

	/**
	 * @return L'ID de la variable
	 */
	public int getId() {
		return id;
	}

	public long getNbSupports(final int index) {
		long nbSupports = 0;
		for (Constraint c : constraints) {
			final long nbSup = c.getNbSupports(this, index);
			if (nbSup <= 0) {
				return -1;
			}
			nbSupports += nbSup;
		}
		return nbSupports;
	}

	public Constraint[] getInvolvingConstraints() {
		return constraints;
	}

	public DynamicConstraint[] getDynamicConstraints() {
		return dynamicConstraints;
	}
	
	public int[] getDomain() {
		return domain;
	}

	public int getFirst() {
		if (assigned) {
			return assignedIndex;
		}
		// assert isPresent(chain.getFirst());
		return chain.getFirst();

	}

	public int getLast() {
		return assigned ? assignedIndex : chain.getLast();
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
		for (int i = getFirst(); i >= 0; i = getNext(i)) {
			remove(i, level);
		}
	}

	public Collection<Integer> getCurrentDomain() {
		final Collection<Integer> values = new ArrayList<Integer>();

		for (int i = getFirst(); i >= 0; i = getNext(i)) {
			values.add(domain[i]);
		}

		return values;
	}

	public String getName() {
		return name;
	}

	public int getRemovedLevel(final int index) {
		return removed[index];
	}

	public int getNext(final int index) {
		if (assigned) {
			int i = chain.getNext(index);
			while (i >= 0 && i != assignedIndex) {
				i = chain.getNext(i);
			}
			return i;
		}

		return chain.getNext(index);
	}

	public int getLastAbsent() {
		return absents.getLast();
	}

	public int getPrevAbsent(final int index) {
		return absents.getPrev(index);
	}

	public int getFirstAbsent() {
		return absents.getFirst();
	}

	public int getNextAbsent(final int index) {
		return absents.getNext(index);
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

	public Variable clone() throws CloneNotSupportedException {
		final Variable variable = (Variable) super.clone();

		variable.removed = removed.clone();

		variable.booleanDomain = booleanDomain.clone();
		variable.beforeAssignDomain = booleanDomain.clone();

		variable.chain = chain.clone();

		variable.absents = absents.clone();

		return variable;
	}

	public void reAssign(final int index) {
		assignedIndex = index;
	}

	public int getPositionInConstraint(final int constraint) {
		return positionInConstraint[constraint];
	}

	public void assignBoolean(final int index) {
		BooleanArray.setSingle(booleanDomain, index);
	}

	public int getNextPresent(int i) {
		int j = getNext(i);
		while (j >= 0 && !isPresent(j)) {
			j = getNext(j);
		}
		return j;
	}
}
