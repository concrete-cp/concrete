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
package cspfj.filter;

import java.util.logging.Logger;

import cspfj.AbstractSolver;
import cspfj.problem.Problem;
import cspfj.problem.Variable;

/**
 * @author Julien VION
 * 
 */
public final class B3CPart extends AbstractSAC {

	// private final static Logger logger =
	// Logger.getLogger("cspfj.filter.CDC");

	private final int parts ;

	private static final Logger logger = Logger.getLogger(AbstractSAC.class
			.toString());

	public B3CPart(Problem problem, Filter filter) {
		super(problem, filter);
		if (AbstractSolver.parameters.containsKey("parts")) {
			parts = Integer.valueOf(AbstractSolver.parameters.get("parts"));
		} else {
			parts = 10;
		}
	}

	protected boolean singletonTest(final Variable variable, final int level) {
		boolean changedGraph = false;
		final int partSize = (int) Math.ceil((double) variable.getDomainSize()
				/ parts);

		logger.finer("Level " + level + ", " + variable + ", part=" + partSize);

		do {
			if (testPart(variable, level, partSize, true)) {
				changedGraph = true;
			} else {
				break;
			}
		} while (variable.getDomainSize() > 0);

		if (variable.getDomainSize() > partSize) {
			do {
				if (testPart(variable, level, partSize, false)) {
					changedGraph = true;
				} else {
					break;
				}
			} while (variable.getDomainSize() > 0);

		}

		return changedGraph;
	}

	/**
	 * M�thode qui traite une tranche.
	 * 
	 * @param variable
	 *            la variable � traiter
	 * @param level
	 *            niveau de l'arbre en cours
	 * 
	 * @param partSize
	 *            la taille de la tranche (nombre de valeurs)
	 * @param start
	 *            vrai si on traite la tranche inf�rieure, faux pour traiter la
	 *            tranche sup�rieure
	 * @return vrai si la tranche a �t� supprim�e
	 */
	private boolean testPart(final Variable variable, final int level,
			final double partSize, final boolean start) {

		if (variable.getDomainSize() > partSize) { // Pas de travail s'il n'y a
			// qu'une tranche

			/*
			 * On supprime toutes les valeurs en dehors de la tranche. On peut
			 * parcourir toutes les valeurs d'un domaine (en fait ce sont les
			 * index) en utilisant les m�thodes du type : for (int i =
			 * variable.getFirst() ; i != 0 ; i = variable.getNext(i)) {}, ou
			 * dans le sens inverse avec getLast() et getPrev().
			 */
			int i = start ? variable.getFirst() : variable.getLast();
			int j = 0;

			// On ne fait rien pour les valeurs dans la tranche en cours...
			while (i >= 0 && j < partSize) {
				i = start ? variable.getNext(i) : variable.getPrev(i);
				j++;
			}

			// Et on supprime le reste...
			while (i >= 0) {
				variable.remove(i, level + 1);
				i = start ? variable.getNext(i) : variable.getPrev(i);
			}
		}

		// Appel au filtre "sous-jacent" (AC, 2B...)
		final boolean test = filter.reduceAfter(level + 1, variable);

		// Restauration des valeurs supprim�es (valeurs hors de la tranche et
		// valeurs supprim�es par le filtre)
		problem.restore(level + 1);

		if (!test) { // Si le filtre a d�tect� une inconsistance...
			int i = start ? variable.getFirst() : variable.getLast();
			int j = 0;
			// On supprime d�finitivement (au niveau en cours) les valeurs de la
			// tranche
			while (i >= 0 && j < partSize) {
				variable.remove(i, level);
				i = start ? variable.getNext(i) : variable.getPrev(i);
				j++;
			}
			return true;
		}

		return false;
	}

	public String toString() {
		return "3B-parts-" + parts + " w/ " + filter;
	}

}
