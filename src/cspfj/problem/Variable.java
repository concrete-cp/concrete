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
import cspfj.util.BitVector;

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
	// private int[] removed;
	private long[] booleanDomain;

	private long[][] history;

	/**
	 * Taille actuelle du domaine.
	 */
	private int domainSize;

	private int[] domainSizeHistory;

	private int currentLevel;

	private long[] savedLevels;

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
		// this.removed = new int[domain.length];
		// Arrays.fill(removed, -1);

		history = new long[1][BitVector.nbWords(domainSize)];
		domainSizeHistory = new int[1];
		booleanDomain = BitVector.newBitVector(domainSize, true);
		currentLevel = 0;
		this.assignedIndex = -1;
		assigned = false;
		id = nbV++;
		savedLevels = BitVector.newBitVector(1, false);
		if (name == null) {
			this.name = "X" + id;
		} else {
			this.name = name;
		}

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
		return assigned ? assignedIndex == index : BitVector.get(booleanDomain,
				index);
	}

	/**
	 * @param index
	 *            La valeur à assigner
	 */
	public void assign(final int index, final int level, final Problem problem) {
		assert !assigned;
		assert isPresent(index);

		assignedIndex = index;
		assigned = true;
		save(level);
		BitVector.setSingle(booleanDomain, index);
		problem.decreaseFutureVariables();
	}

	private void save(int level) {
		if (level <= currentLevel) {
			return;
		}
		final int oldLevel = currentLevel;
		if (oldLevel>= history.length) {
			history = Arrays.copyOf(history, oldLevel + 1);
			history[oldLevel] = booleanDomain.clone();
			domainSizeHistory = Arrays.copyOf(domainSizeHistory, oldLevel+ 1);
			savedLevels = Arrays.copyOf(savedLevels, BitVector
					.nbWords(oldLevel+ 1));
		} else if (history[oldLevel] == null) {
			history[oldLevel] = booleanDomain.clone();
		} else {
			System.arraycopy(booleanDomain, 0, history[oldLevel], 0,
					booleanDomain.length);
		}
		domainSizeHistory[oldLevel] = domainSize;
		BitVector.set(savedLevels, oldLevel);
		currentLevel = level;
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
	}

	/**
	 * @param level
	 *            Le niveau à restaurer
	 * @return True si au moins une valeur a été restaurée
	 */
	public void restore(final int level) {
		assert !assigned;
		final int levelToRestore = BitVector.prevSetBit(savedLevels, level + 1);
		if (levelToRestore >= 0) {
			BitVector.clear(savedLevels, level+1, 64 * savedLevels.length);
			System.arraycopy(history[levelToRestore], 0, booleanDomain, 0,
					booleanDomain.length);
			domainSize = domainSizeHistory[levelToRestore];
		}
		currentLevel = level;
	}

	// private void restore(final int index) {
	// assert !isPresent(index);
	// domainSize++;
	// removed[index] = -1;
	// BitVector.set(booleanDomain, index);
	// }

	/**
	 * @param index
	 *            L'index à supprimer
	 * @param level
	 *            Le niveau en cours
	 */
	public void remove(final int index, final int level) {
		assert !assigned;
		assert isPresent(index);

		save(level);

		// removed[index] = level;
		BitVector.clear(booleanDomain, index);
		domainSize--;
	}

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
		return assigned ? assignedIndex : BitVector
				.nextSetBit(booleanDomain, 0);
	}

	public int getLast() {
		return assigned ? assignedIndex : BitVector.prevSetBit(booleanDomain,
				domain.length);
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

	// public int getRemovedLevel(final int index) {
	// return removed[index];
	// }

	public long[] getDomainAtLevel(final int level) {
		final int levelToRestore = BitVector.nextSetBit(savedLevels, level);
		if (levelToRestore < 0) {
			return null;
		}
		return history[levelToRestore];
	}

	public int getNext(final int index) {
		return BitVector.nextSetBit(booleanDomain, index + 1);
	}

	public int getPrev(final int index) {
		return BitVector.prevSetBit(booleanDomain, index);
	}

	public int getLastAbsent() {
		return BitVector.prevClearBit(booleanDomain, domain.length);
	}

	public int getPrevAbsent(final int index) {
		return BitVector.prevClearBit(booleanDomain, index);
	}

	public long[] getBitDomain() {
		return booleanDomain;
	}

	public Variable clone() throws CloneNotSupportedException {
		final Variable variable = (Variable) super.clone();

		variable.booleanDomain = booleanDomain.clone();
		return variable;
	}

	public void reAssign(final int index) {
		assignedIndex = index;
	}

	public int getPositionInConstraint(final int constraint) {
		return positionInConstraint[constraint];
	}

	public int getNextPresent(int i) {
		int j = getNext(i);
		while (j >= 0 && !isPresent(j)) {
			j = getNext(j);
		}
		return j;
	}
}
