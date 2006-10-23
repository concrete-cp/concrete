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
import java.util.SortedMap;
import java.util.TreeMap;
import java.util.TreeSet;

import cspfj.TabuManager;
import cspfj.constraint.Constraint;
import cspfj.util.BooleanArray;
import cspfj.util.TieManager;

/**
 * @author scand1sk
 * 
 */
public final class Variable implements Comparable<Variable> {
    /**
     * Contraintes impliquant la variable.
     */
    private Constraint involvingConstraints[];

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
    private final int[] removed;

    private final int[] next;

    private final int[] prev;

    private final int[] prevAbsents;

    private final int[] booleanDomain;

    private final int[] beforeAssignDomain;

    /**
     * Taille actuelle du domaine.
     */
    private int domainSize;

    /**
     * Variable assignée ?
     */
    private int assignedIndex;

    /**
     * ID de la variable.
     */
    private final int id;

    /**
     * Pour générer l'ID.
     */
    private static int nbV = 0;

    private final int[] order;

    private final double[] valueHeuristic;

    private int firstPresentIndex;

    private int lastPresentIndex;

    private int lastAbsentIndex;

    
    
    // private boolean selectable = true;

    private final String name;

//    private static final Logger logger = Logger.getLogger("cspfj.Variable");

    private static final TieManager<Integer, Integer> tieManager = new TieManager<Integer, Integer>(
            -1, Integer.MAX_VALUE);

    // private final DomainIterator iterator;

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
        for (int i = 0; i < removed.length; i++) {
            removed[i] = -1;
        }
        booleanDomain = BooleanArray.newBooleanArray(domainSize, true);
        beforeAssignDomain = booleanDomain.clone();
        this.assignedIndex = -1;
        id = nbV++;
        firstPresentIndex = 0;
        lastPresentIndex = domain.length - 1;
        lastAbsentIndex = -1;
        order = new int[domain.length];
        valueHeuristic = new double[domain.length];

        Arrays.fill(valueHeuristic, Double.MAX_VALUE / 2);

        this.name = name;

        this.next = new int[domain.length];
        this.prev = new int[domain.length];

        for (int i = 0; i < domain.length - 1; i++) {
            next[i] = i + 1;
            prev[i + 1] = i;
        }
        prev[0] = next[domain.length - 1] = -1;
        this.prevAbsents = new int[domain.length];
        // Arrays.fill(prevAbsents, -1);

        // iterator = new DomainIterator(this);
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

    /**
     * @param constraints
     *            Liste des contraintes impliquant la variable
     */
    public void setInvolvingConstraints(final Constraint[] constraints) {
        this.involvingConstraints = constraints.clone();

        final Set<Variable> neighb = new TreeSet<Variable>();

        for (Constraint c : involvingConstraints) {
            for (Variable v : c.getInvolvedVariables()) {
                if (v == this) {
                    continue;
                }
                // if (neighb.contains(v)) {
                // continue;
                // }
                neighb.add(v);
            }
        }

        this.neighbours = neighb.toArray(new Variable[neighb.size()]);
    }

    /**
     * @return La taille du domaine
     */
    public int getDomainSize() {
        return isAssigned() ? 1 : domainSize;
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
        // throw new NotInDomainException(this + ", " + value);
    }

    /**
     * @return L'état (assignée ou non) de la variable
     */
    public boolean isAssigned() {
        return assignedIndex != -1;
    }

    /**
     * @param level
     *            Le niveau en cours de la recherche
     * @return True si au moins une valeur a été révisée (supprimée)
     */
    public Constraint revise(final int level) {
        assert !isAssigned();

        Constraint revised = null;

        // for (int index = 0; index < getDomain().length; index++) {
        // if (isPresent(index)) {

        for (Constraint c : involvingConstraints) {
            // System.out.println("seeking in "+c);
            if (c.revise(this, level)) {
                revised = c;
                if (getDomainSize() <= 0) {
                    break;
                }
            }
        }
        // }
        // }

        return revised;
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
        return assignedIndex == index
                || (!isAssigned() && removed[index] == -1);
    }



    /**
     * @param index
     *            La valeur à assigner
     * @param level
     *            Le niveau en cours
     */
    public void assign(final int index, final Problem problem) {
        assert !isAssigned();
        assert isPresent(index);

        assignedIndex = index;
        System.arraycopy(booleanDomain, 0, beforeAssignDomain, 0,
                booleanDomain.length);
        BooleanArray.setSingle(booleanDomain, index);
        assert !reinitBooleanDomain();
        problem.decreaseFutureVariables();
    }

    /**
     * @param level
     *            Le niveau en cours
     */
    public void unassign(final Problem problem) {
        assert isAssigned();
        assignedIndex = -1;
        problem.increaseFutureVariables();
        System.arraycopy(beforeAssignDomain, 0, booleanDomain, 0,
                booleanDomain.length);
        // problem.restore(level);
    }

    /**
     * @param level
     *            Le niveau à restaurer
     * @return True si au moins une valeur a été restaurée
     */
    public void restoreLevel(final int level) {
        assert !isAssigned();

        // boolean restored = false;

        final int[] removed = this.removed;
        final int[] prevAbsents = this.prevAbsents;

        for (int i = lastAbsentIndex; i >= 0; i = prevAbsents[i]) {
            if (removed[i] >= level) {
                restore(i);
            }
        }

        // return restored;
    }

    private void restore(final int index) {
        assert !isPresent(index);

        final int[] removed = this.removed;
        final int[] prevAbsents = this.prevAbsents;

        // add to the list of present elements
        domainSize++;
        removed[index] = -1;
        BooleanArray.set(booleanDomain, index, true);
        int next = -1;
        for (int i = index + 1; next == -1 && i < removed.length; i++) {
            if (removed[i] == -1) {
                next = i;
            }
        }

        int prev = -1;
        for (int i = index - 1; prev == -1 && i >= 0; i--) {
            if (removed[i] == -1) {
                prev = i;
            }
        }

        this.prev[index] = prev;
        this.next[index] = next;

        if (prev == -1) {
            firstPresentIndex = index;
        } else {
            this.next[prev] = index;
        }
        if (next == -1) {
            lastPresentIndex = index;
        } else {
            this.prev[next] = index;
        }

        // remove from the list of absent elements

        if (lastAbsentIndex == index) {
            lastAbsentIndex = prevAbsents[index];
        } else {
            int current = lastAbsentIndex;
            while (prevAbsents[current] != index) {
                current = prevAbsents[current];
            }
            prevAbsents[current] = prevAbsents[index];
        }
    }

    /**
     * @param index
     *            L'index à supprimer
     * @param level
     *            Le niveau en cours
     */
    public void remove(final int index, final int level) {
        // System.out.println("Removing " +domain[index]+ " from " + this);
        assert !isAssigned();
        assert isPresent(index);
        removed[index] = level;
        BooleanArray.set(booleanDomain, index, false);

        domainSize--;
        final int next = this.next[index];
        final int prev = this.prev[index];
        if (prev == -1) {
            firstPresentIndex = next;
        } else {
            this.next[prev] = next;
        }
        if (next == -1) {
            lastPresentIndex = prev;
        } else {
            this.prev[next] = prev;
        }

        prevAbsents[index] = lastAbsentIndex;
        lastAbsentIndex = index;

        // if (level == 0) {
        // for (Constraint c : involvingConstraints) {
        // c.freeLast(this, index);
        // }
        // }

    }

    /**
     * @return L'ID de la variable
     */
    public int getId() {
        return id;
    }

    private final static SortedMap<Double, Integer> supports = new TreeMap<Double, Integer>();

    private static double poisson(final double moy, final Random random) {
        return -moy * Math.log(random.nextDouble());
    }

//    public void orderIndexes(final Random random) {
//
//        supports.clear();
//        final double[] valueHeuristic = this.valueHeuristic;
//        logger.finer("Counting supports");
//        for (int i : this) {
//
//            double r;
//            do {
//                r = poisson(valueHeuristic[i], random);
//            } while (supports.containsKey(r));
//
//            supports.put(r, i);
//
//        }
//
//        logger.finer("Ordering");
//
//        final int[] order = this.order;
//        int o = 0;
//
//        for (int i : supports.values()) {
//            order[o++] = i;
//        }
//
//    }

    public void setValueHeuristic(final int index, final double value) {
        valueHeuristic[index] = value;
    }

    public int getNbSupports(final int index) {
        int nbSupports = 0;
        for (Constraint c : involvingConstraints) {
            if (c.getArity() <= 3) {
                nbSupports += c.getNbSupports(this, index);
            }
        }
        return nbSupports;
    }

    public Constraint[] getInvolvingConstraints() {
        return involvingConstraints;
    }

    public int[] getDomain() {
        return domain;
    }

    public int getFirst() {
        return isAssigned() ? assignedIndex : firstPresentIndex;

    }

    // public int getNextPresentIndex(final int index) {
    // for (int i = index + 1; i < domain.length; i++) {
    // if (isPresent(i)) {
    // return i;
    // }
    // }
    //
    // return -1;
    // }

    public float getWDeg() {
        float count = 0;
        if (involvingConstraints == null) {
            return 1;
        }

        for (Constraint c : involvingConstraints) {
            count += c.getFreedomDegree() * c.getWeight();
        }
        return count;
    }

    public float getDDeg() {
        float count = 0;
        if (involvingConstraints == null) {
            return 1;
        }

        for (Constraint c : involvingConstraints) {
            count += c.getFreedomDegree();
        }
        return count;
    }

    public int compareTo(final Variable arg0) {
        final int compare = domainSize - arg0.getDomainSize();

        if (compare == 0) {
            final float compareDeg = arg0.getWDeg() - getWDeg();
            if (compareDeg == 0) {
                return id - arg0.getId();
            }
            return compareDeg > 0 ? 1 : -1;
        }
        return compare;
    }

    public void makeSingleton(final int value, final int level) {
        final int[] domain = this.domain;
        for (int i = getFirst() ; i >=0 ; i = getNext(i)) {
            if (domain[i] != value) {
                remove(i, level);
            }
        }
    }

    public void makeSingletonIndex(final int sol, final int level) {
        for (int i = getFirst() ; i >=0 ; i = getNext(i)) {
            if (i != sol) {
                remove(i, level);
            }
        }

    }

    public void empty(final int level) {

        if (getDomainSize() > 0) {
            for (int i = getFirst() ; i >=0 ; i = getNext(i)) {

                remove(i, level);

            }
        }

    }

    public Integer[] getCurrentDomain() {
        final List<Integer> values = new ArrayList<Integer>();

        for (int i = getFirst() ; i >=0 ; i = getNext(i)) {
            values.add(domain[i]);
        }

        return values.toArray(new Integer[values.size()]);
    }

    // public boolean isSelectable() {
    // return selectable;
    // }
    //
    // public void setSelectable(final boolean isSelectable) {
    // this.selectable = isSelectable;
    // }

    public String getName() {
        return name;
    }

    public int assignRandomPresentIndex(final Random random,
            final Problem problem) {
        int index;
        do {
            index = random.nextInt(domain.length);
        } while (!isPresent(index));

        assign(index, problem);
        return index;
    }

    private int bestIndex;

    // private int bestImp;

    public int bestImprovment(final TabuManager tabuManager,
            final Random random, final int aspiration) {
        if (bestIndex > 0 && !tabuManager.isTabu(this, bestIndex)) {
            return bestIndex;
        }

        final int[] order = this.order;

        tieManager.clear();
        for (int i = domain.length; --i >= 0;) {
            if (order[i] != Integer.MAX_VALUE
                    && (order[i] < aspiration || !tabuManager.isTabu(this, i))) {

                tieManager.newValue(i, order[i], random);
            }
        }

        bestIndex = tieManager.getBestValue();
        // bestImp = tieManager.getBestEvaluation();
        return bestIndex;
    }

    public int getImprovment(final int index) {
        return order[index];
    }

    private int nbConflicts;

    public int getBestInitialIndex(final Random random) {
        if (assignedIndex >= 0) {
            return assignedIndex;
        }

        final TieManager<Integer, Integer> tieManager = Variable.tieManager;
        final int[] removed = this.removed;
        final Constraint[] involvingConstraints = this.involvingConstraints;

        tieManager.clear();

        for (int i = domain.length; --i >= 0;) {
            if (removed[i] >= 0) {
                continue;
            }
            // logger.finest(this + " " + i);
            assignedIndex = i;

            int indexConflicts = 0;

            for (Constraint c : involvingConstraints) {

                if (c.useTupleCache() && !c.findValidTuple(this, i)) {
                    indexConflicts += c.getWeight();
                }

            }

            tieManager.newValue(i, indexConflicts, random);

        }

        assignedIndex = -1;

        bestIndex = tieManager.getBestValue();
        return bestIndex;
    }

    public void updateNbConflicts(final Random random) {

        int nbConflicts = 0;

        for (Constraint c : involvingConstraints) {
            if (!c.checkFirstWith(this, assignedIndex)) {
                nbConflicts += c.getWeight();
            }
        }
        this.nbConflicts = nbConflicts;

        final int oldIndex = assignedIndex;
        final TieManager<Integer, Integer> tieManager = Variable.tieManager;
        final int[] removed = this.removed;
        final int[] order = this.order;
        final Constraint[] involvingConstraints = this.involvingConstraints;

        tieManager.clear();

        for (int i = domain.length; --i >= 0;) {
            if (removed[i] >= 0 || i == oldIndex) {
                order[i] = Integer.MAX_VALUE;
                continue;
            }

            int indexConflicts = 0;

            assignedIndex = i;

            for (Constraint c : involvingConstraints) {
                if (!c.checkFirstWith(this, i)) {
                    indexConflicts += c.getWeight();
                }

            }

            tieManager.newValue(i, indexConflicts - nbConflicts, random);
            order[i] = indexConflicts - nbConflicts;
        }
        // bestImp = tieManager.getBestEvaluation();
        bestIndex = tieManager.getBestValue();

        assignedIndex = oldIndex;

    }

    public int getNbConflicts() {
        return nbConflicts;
    }

    public int getRemovedLevel(final int index) {
        return removed[index];
    }

    public int getNext(final int index) {
        if (isAssigned()) {
            if (index < assignedIndex) {
                return assignedIndex;
            }
            return -1;
        }

        if (isPresent(index)) {
            return next[index];
        }

        if (index < firstPresentIndex) {
            return firstPresentIndex;
        }

        if (index >= lastPresentIndex) {
            return -1;
        }

        for (int i = index + 1; i < domain.length; i++) {
            if (isPresent(i)) {
                return i;
            }
        }
        return -1;

    }

    public int getPrev(final int index) {
        return prev[index];
    }

    public int getLastAbsent() {
        return lastAbsentIndex;
    }

    public int getPrevAbsent(final int index) {
        return prevAbsents[index];
    }

    public int getLastPresent() {
        return lastPresentIndex;
    }

    // public static void main(String[] args) {
    // final Variable v = new Variable(new int[] { 1, 2, 3, 4, 5 });
    // System.out.println(v.displayState());
    // v.remove(2, 0);
    // System.out.println(v.displayState());
    // v.remove(4, 0);
    // System.out.println(v.displayState());
    // v.restore(2);
    // System.out.println(v.displayState());
    // v.remove(0, 0);
    // v.remove(2, 0);
    // System.out.println(v.displayState());
    // v.restoreLevel(0);
    // System.out.println(v.displayState());
    // }

    public String displayState() {
        StringBuffer sb = new StringBuffer();
        sb.append(Arrays.toString(domain)).append('\n');
        sb.append(Arrays.toString(removed)).append('\n');
        for (int i = getFirst() ; i >=0 ; i = getNext(i)) {
            sb.append(i).append(' ');
        }
        sb.append('\n');
        sb.append(firstPresentIndex).append('\n');
        sb.append(Arrays.toString(next)).append('\n');
        sb.append(lastPresentIndex).append('\n');
        sb.append(Arrays.toString(prev)).append('\n');
        sb.append(lastAbsentIndex).append('\n');
        sb.append(Arrays.toString(prevAbsents)).append('\n');
        return sb.toString();
    }

    public int[] getBooleanDomain() {
        return booleanDomain;
    }

    public boolean reinitBooleanDomain() {
        boolean change = false;
        if (isAssigned()) {
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

}
