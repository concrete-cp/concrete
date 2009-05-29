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
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Timer;
import java.util.TreeSet;
import java.util.Map.Entry;
import java.util.logging.Logger;

import cspfj.constraint.AbstractConstraint;
import cspfj.constraint.Constraint;
import cspfj.constraint.DynamicConstraint;
import cspfj.constraint.extension.ExtensionConstraint2D;
import cspfj.constraint.extension.ExtensionConstraintGeneral;
import cspfj.constraint.extension.Matrix;
import cspfj.constraint.extension.Matrix2D;
import cspfj.constraint.extension.TupleSet;
import cspfj.constraint.semantic.RCConstraint;
import cspfj.exception.FailedGenerationException;
import cspfj.util.BitVector;
import cspfj.util.Waker;

public final class Problem implements Cloneable {
    // private Map<Integer, Variable> variables;

    private Variable[] variableList;

    private Variable[] variableMap;

    private int nbVariables = 0;

    private Map<Integer, Constraint> constraints;

    private Constraint[] constraintArray;

    private int nbFutureVariables = 0;

    private final static Logger LOGGER = Logger.getLogger(Problem.class
            .getName());

    private int maxDomainSize;

    private Variable[] levelVariables;

    private int maxArity;

    private String name;

    private boolean useNoGoods;

    private int maxVId;

    private int maxCId;

    private int nbNoGoods = 0;

    private int currentLevel;

    public Problem() {
        super();
        this.useNoGoods = true;// true;
    }

    public Problem(Collection<Variable> variables,
            Collection<Constraint> constraints, String name) {
        this();
        this.name = name;
        setVariables(variables);
        setConstraints(constraints);
        updateInvolvingConstraints();
    }

    public static Problem load(final ProblemGenerator generator)
            throws FailedGenerationException {
        return load(generator, -1);
    }

    public static Problem load(final ProblemGenerator generator,
            final int expCountSupports) throws FailedGenerationException {
        final Problem problem = new Problem();
        Variable.resetVId();
        AbstractConstraint.resetCId();

        LOGGER.info("Generating");
        generator.generate();

        LOGGER.info("Setting Variables");
        problem.setVariables(generator.getVariables());

        LOGGER.info("Counting supports (" + expCountSupports + ")");

        Thread.interrupted();
        final Timer waker = new Timer();
        if (expCountSupports >= 0) {
            waker.schedule(new Waker(Thread.currentThread()),
                    expCountSupports * 1000);
        }

        final Constraint[] sorted = generator.getConstraints().toArray(
                new Constraint[generator.getConstraints().size()]);

        Arrays.sort(sorted, new Comparator<Constraint>() {
            @Override
            public int compare(Constraint o1, Constraint o2) {
                return Double.compare(o1.getInitSize(), o2.getInitSize());
            }
        });

        try {
            for (Constraint c : sorted) {
                c.initNbSupports();
            }
        } catch (InterruptedException e) {
            // Continue...
        } finally {
            waker.cancel();
        }

        LOGGER.info("Setting Constraints");
        problem.setConstraints(generator.getConstraints());

        LOGGER.info("Updating InvolvingConstraints");
        problem.updateInvolvingConstraints();

        // for (Constraint c :
        // generator.getVariables()[21].getInvolvingConstraints()) {
        // if (c.getInvolvedVariables()[1] == generator.getVariables()[23])
        // System.out.println(c);
        // }

        LOGGER.info("Done");
        return problem;

    }

    public int getNbFutureVariables() {
        return nbFutureVariables;
    }

    private void setVariables(final Collection<Variable> vars) {

        // this.variables = new HashMap<Integer, Variable>(vars.size());

        // this.variableArray = vars.toArray(new Variable[vars.size()]);

        maxDomainSize = 0;

        maxVId = 0;

        for (Variable var : vars) {
            maxDomainSize = Math.max(maxDomainSize, var.getDomain().maxSize());
            maxVId = Math.max(maxVId, var.getId());
        }

        this.variableMap = new Variable[maxVId + 1];
        this.variableList = new Variable[vars.size()];
        int listTail = 0;
        for (Variable var : vars) {
            variableMap[var.getId()] = var;
            variableList[listTail++] = var;
        }

        nbFutureVariables = nbVariables = vars.size();

        levelVariables = new Variable[getNbVariables()];
        Arrays.fill(levelVariables, null);

    }

    public void setConstraints(final Collection<Constraint> constraints2) {
        this.constraints = new HashMap<Integer, Constraint>(constraints2.size());

        this.constraintArray = constraints2.toArray(new Constraint[constraints2
                .size()]);

        maxCId = 0;

        for (Constraint c : constraints2) {
            // if (c.getArity() == 1) {
            // final Variable variable = c.getInvolvedVariables()[0];
            // final int[] tuple = c.getTuple();
            // for (int i = variable.getFirst(); i >= 0; i = variable
            // .getNext(i)) {
            // tuple[0] = i;
            // if (!c.check()) {
            // variable.remove(i, 0);
            // }
            //
            // }
            // continue ;
            // }
            this.constraints.put(c.getId(), c);
            if (c.getArity() > maxArity) {
                maxArity = c.getArity();
            }
            if (c.getId() > maxCId) {
                maxCId = c.getId();
            }
        }

        // resetConstraint = new boolean[constraints.length];
    }

    public void updateInvolvingConstraints() {
        final Map<Integer, List<Constraint>> invConstraints = new HashMap<Integer, List<Constraint>>(
                variableList.length);

        for (Variable v : getVariables()) {
            if (v != null) {
                invConstraints.put(v.getId(), new ArrayList<Constraint>());
            }
        }

        for (Constraint c : getConstraints()) {
            for (Variable v : c.getScope()) {
                invConstraints.get(v.getId()).add(c);
            }
        }

        for (Variable v : getVariables()) {
            if (v != null) {
                final Collection<Constraint> involvingConstraints = invConstraints
                        .get(v.getId());
                v.setInvolvingConstraints(involvingConstraints
                        .toArray(new Constraint[involvingConstraints.size()]));
            }
        }

        // for (Constraint c : getConstraints()) {
        // c.initNbSupports();
        // }

        // final ValueHeuristic maxS = new Supports(this, false);
        // maxS.compute();

        // for (Variable v: getVariables()) {
        // v.heuristicToOrder();
        // }
    }

    public int getNbVariables() {
        return nbVariables;
    }

    public int getNbConstraints() {
        return constraints.size();
    }

    public Constraint[] getConstraints() {
        return constraintArray;
    }

    public Collection<Constraint> getConstraintsAsCollection() {
        return constraints.values();
    }

    // public Collection<Integer> getConstraintIDs() {
    // return constraints.keySet();
    // }

    public Constraint getConstraint(final int cId) {
        return constraints.get(cId);
    }

    public Variable[] getVariables() {
        return variableList;
    }

    // public Collection<Integer> getVariableIDs() {
    // return variables.keySet();
    // }

    public Variable getVariable(final int vId) {
        return variableMap[vId];
    }

    public void increaseFutureVariables() {
        nbFutureVariables++;
    }

    public void decreaseFutureVariables() {
        nbFutureVariables--;
    }

    public void push() {
        currentLevel++;
        setLevel(currentLevel);

    }

    public void pop() {
        currentLevel--;
        restoreLevel(currentLevel);

    }

    private void setLevel(int level) {
        // currentLevel = level;
        for (Variable v : variableList) {
            v.setLevel(level);
        }
        for (Constraint c : constraintArray) {
            c.setLevel(level);
        }
    }

    private void restoreLevel(int level) {
        // currentLevel = level;
        for (Variable v : variableList) {
            v.restoreLevel(level);
        }
        for (Constraint c : getConstraints()) {
            c.restore(level);
        }
    }

    public void reset() {
        currentLevel = 0;
        for (Variable v : variableList) {
            v.reset(this);
        }
        for (Constraint c : getConstraints()) {
            c.restore(0);
        }
    }

    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder();
        for (Variable v : getVariables()) {
            sb.append(v).append(" : ").append(
                    Arrays.toString(v.getCurrentIndexes())).append('\n');
        }

        for (Constraint c : constraintArray) {
            sb.append(c.toString()).append('\n');
        }

        return sb.toString();
    }

    public int getMaxDomainSize() {
        return maxDomainSize;
    }

    private final static DynamicConstraint findDynamicConstraint(
            final Set<Variable> scope) {

        for (DynamicConstraint c : scope.iterator().next()
                .getDynamicConstraints()) {
            if (c.getArity() == scope.size()
            // || (c.getArity() > scope.size() && c
                    // .positive()))
                    && c.getScopeSet().containsAll(scope)) {
                return c;
            }
        }
        return null;
    }

    public final DynamicConstraint learnConstraint(final Set<Variable> scope,
            final LearnMethod addConstraints) {
        final DynamicConstraint constraint = findDynamicConstraint(scope);

        if (constraint != null) {
            return constraint;
        }

        if (addConstraints == LearnMethod.NONE) {
            return null;
        }

        final int level = scope.size();
        final Variable[] constraintScope = scope.toArray(new Variable[level]);

        if (level == 2) {

            if (addConstraints == LearnMethod.RC) {
                return new RCConstraint(constraintScope);
            }

            final Matrix2D matrix = new Matrix2D(constraintScope[0].getDomain()
                    .maxSize(), constraintScope[1].getDomain().maxSize(), true);
            return new ExtensionConstraint2D(constraintScope, matrix, false,
                    true);

        }

        if (addConstraints == LearnMethod.EXT) {

            final Matrix matrix = new TupleSet(true);

            return new ExtensionConstraintGeneral(matrix, false, true,
                    constraintScope);

        }
        return null;
    }

    private static int[] makeBase(Variable[] scope, int[] values,
            Constraint constraint) {
        assert scope.length == values.length;
        int[] tuple = new int[constraint.getArity()];

        Arrays.fill(tuple, -1);

        for (int i = constraint.getArity(); --i >= 0;) {
            final Variable var = constraint.getVariable(i);
            for (int j = scope.length; --j >= 0;) {
                if (scope[j] == var) {
                    tuple[i] = values[j];
                    break;
                }
            }
        }

        return tuple;
    }

    /**
     * Sets the base array given as a parameter so that the values of base
     * correspond to the values of the values array reordered such that they
     * correspond to the variables of the scope of the constraint. Variables
     * present in the scope of the constraint but not in the scope[] array
     * result in a -1 value in the base[] array. Last variable of scope[] is
     * ignored. Returns the position of the last variable of scope[] in the
     * constraint's scope.
     * 
     * @param scope
     * @param values
     * @param constraint
     * @param base
     * @return
     */
    public static int makeBase(Variable[] scope, int[] values,
            Constraint constraint, int[] base) {
        assert scope.length == values.length;
        assert base.length == constraint.getArity();

        Arrays.fill(base, -1);

        final Variable seek = scope[scope.length - 1];
        int positionInConstraint = -1;

        for (int i = constraint.getArity(); --i >= 0;) {
            final Variable var = constraint.getVariable(i);
            if (var == seek) {
                positionInConstraint = i;
                continue;
            }
            for (int j = scope.length - 1; --j >= 0;) {
                if (scope[j] == var) {
                    base[i] = values[j];
                    break;
                }
            }

        }

        return positionInConstraint;
    }

    public int getCurrentLevel() {
        return currentLevel;
    }

    public boolean noGoods(LearnMethod learnMethod) {
        if (!useNoGoods) {
            return false;
        }

        final Variable[] levelVariables = this.levelVariables;

        if (levelVariables[0] == null) {
            return false;
        }

        // final Map<Variable[], List<int[]>> noGoods = new HashMap<Variable[],
        // List<int[]>>();

        int startLevel = 0;
        for (; startLevel < levelVariables.length
                && levelVariables[startLevel] != null; startLevel++)
            ;

        int[] tuple = new int[startLevel + 1];

        final Set<Variable> scopeSet = new HashSet<Variable>(startLevel);
        for (int i = startLevel; --i >= 0;) {
            scopeSet.add(levelVariables[i]);
            tuple[i] = levelVariables[i].getFirst();
        }

        boolean modified = false;
        final Collection<Constraint> addedConstraints = new ArrayList<Constraint>();

        for (int level = startLevel + 1; --level >= 1;) {
            // Note : Nothing to remove on first level
            scopeSet.remove(levelVariables[level]);
            final Variable[] scopeArray = Arrays.copyOf(levelVariables,
                    level + 1);
            // restoreLevel(level);

            tuple = Arrays.copyOf(tuple, level + 1);

            for (Variable fv : variableList) {

                // logger.fine("checking " +
                // getVariable(levelVariables[level-1]));

                if (scopeSet.contains(fv)) {
                    continue;
                }

                final BitVector changes = fv.getDomain().getAtLevel(level - 1)
                        .xor(fv.getDomain().getAtLevel(level));
                if (changes.isEmpty()) {
                    continue;
                }

                scopeSet.add(fv);
                final DynamicConstraint constraint = learnConstraint(scopeSet,
                        learnMethod);
                scopeSet.remove(fv);

                if (constraint == null) {
                    continue;
                }

                scopeArray[level] = fv;

                final int[] base = new int[constraint.getArity()];
                final int varPos = makeBase(scopeArray, tuple, constraint, base);

                int newNogoods = 0;
                for (int i = changes.nextSetBit(0); i >= 0; i = changes
                        .nextSetBit(i + 1)) {
                    base[varPos] = i;
                    newNogoods += constraint.removeTuples(base);

                }
                if (newNogoods > 0) {
                    nbNoGoods += newNogoods;
                    modified = true;
                    if (constraint.getId() > getMaxCId()) {
                        LOGGER.info("Added " + constraint);
                        addedConstraints.add(constraint);
                    }
                }
            }
        }
        if (modified) {
            LOGGER.info(nbNoGoods + " nogoods");

            if (!addedConstraints.isEmpty()) {
                final Collection<Constraint> curCons = new ArrayList<Constraint>(
                        constraints.values());

                for (Constraint c : addedConstraints) {
                    curCons.add(c);
                }

                setConstraints(curCons);
                updateInvolvingConstraints();
                LOGGER.info(getNbConstraints() + " constraints");
            }
        }
        return modified;
    }

    public boolean noGoodsToConstraints(Map<Variable[], List<int[]>> noGoods,
            final LearnMethod learnMethod) {
        if (noGoods == null) {
            return false;
        }
        boolean modified = false;
        final Collection<Constraint> addedConstraints = new ArrayList<Constraint>();

        for (Entry<Variable[], List<int[]>> e : noGoods.entrySet()) {
            final Set<Variable> scope = new HashSet<Variable>(Arrays.asList(e
                    .getKey()));

            final DynamicConstraint constraint = learnConstraint(scope,
                    learnMethod);

            if (constraint == null) {
                continue;
            }

            int newNogoods = 0;
            for (int[] tuple : e.getValue()) {
                newNogoods += constraint.removeTuples(makeBase(e.getKey(),
                        tuple, constraint));
            }
            if (newNogoods == 0) {
                continue;
            }
            nbNoGoods += newNogoods;
            modified = true;
            if (constraint.getId() > getMaxCId()) {
                LOGGER.info("Added " + constraint);
                addedConstraints.add(constraint);
            }

        }

        if (modified) {
            LOGGER.info(nbNoGoods + " nogoods");

            if (!addedConstraints.isEmpty()) {
                final Collection<Constraint> curCons = new ArrayList<Constraint>(
                        constraints.values());

                for (Constraint c : addedConstraints) {
                    curCons.add(c);
                }

                setConstraints(curCons);
                updateInvolvingConstraints();
                LOGGER.info(getNbConstraints() + " constraints");
            }
        }
        return modified;
    }

    public int getNbNoGoods() {
        return nbNoGoods;
    }

    public void setLevelVariables(final Variable variable) {
        assert (currentLevel + 1 >= levelVariables.length || levelVariables[currentLevel + 1] == null);
        levelVariables[currentLevel] = variable;

    }

    public Variable getLevelVariable(final int level) {
        return levelVariables[level];
    }

    public void clearLevelVariables() {
        Arrays.fill(levelVariables, null);
    }

    public int[] getLevelVariables(final int minLevel) {
        final int[] variables = new int[levelVariables.length - minLevel];
        System.arraycopy(levelVariables, minLevel, variables, 0,
                variables.length);
        return variables;
    }

    public int getMaxArity() {
        return maxArity;
    }

    public static Problem activeProblem(final Problem problem) {
        return activeProblem(problem, 0);
    }

    public static Problem activeProblem(final Problem problem,
            final int additionalConstraints) {
        final Collection<Constraint> constraints = new ArrayList<Constraint>();

        final Collection<Constraint> otherConstraints = new ArrayList<Constraint>();

        final Set<Variable> activeVariables = new TreeSet<Variable>();

        for (Constraint c : problem.getConstraints()) {
            if (c.isActive()) {
                constraints.add(c);
                for (Variable v : c.getScope()) {
                    activeVariables.add(v);
                }
                c.setActive(false);
            } else {
                otherConstraints.add(c);
            }

        }

        final Constraint[] sortedConstraints = otherConstraints
                .toArray(new Constraint[otherConstraints.size()]);

        Arrays.sort(sortedConstraints, new cspfj.constraint.Weight(true));

        int i = additionalConstraints;
        for (Constraint c : sortedConstraints) {
            if (i-- <= 0) {
                break;
            }
            constraints.add(c);
            for (Variable v : c.getScope()) {
                activeVariables.add(v);
            }
        }

        return new Problem(activeVariables, constraints, problem.getName());

    }

    public String getName() {
        return name;
    }

    public void setName(final String name) {
        this.name = name;
    }

    public void setUseNoGoods(final boolean b) {
        this.useNoGoods = b;
    }

    public int getMaxVId() {
        return maxVId;
    }

    public int getMaxCId() {
        return maxCId;
    }

    public int getND() {
        int nd = 0;
        for (Variable v : getVariables()) {
            nd += v.getDomainSize();
        }
        return nd;
    }

    public int getMaxFlips() {
        final int nd = getND();
        // return 8 * nd + (int)(.4 * nd * nd);
        // return 5*nd;
        return Math.max((int) (-50000 + 10000 * Math.log(nd)), 10000);
    }

    public int getMaxBacktracks() {
        // final int meanDomainSize = getND() / getNbVariables();
        //
        // final int localBT = getMaxFlips();
        //
        // return (int) (localBT * (100F * getNbVariables()) /
        // (getNbConstraints() * meanDomainSize));
        return Math.max(10, maxDomainSize / 10);
    }

    public Problem clone() throws CloneNotSupportedException {
        final Problem problem = (Problem) super.clone();
        // problem.variables = null;
        //
        // problem.variableArray = null;
        //
        // problem.constraints = null;
        //
        // problem.constraintArray = null;
        //
        // problem.levelVariables = null;

        final Collection<Variable> variables = new ArrayList<Variable>(this
                .getNbVariables());

        for (Variable v : this.getVariables()) {
            variables.add(v.clone());
        }

        problem.setVariables(variables);

        final Collection<Constraint> constraints = new ArrayList<Constraint>(
                this.getNbConstraints());

        for (Constraint c : this.getConstraints()) {
            constraints.add(c.deepCopy(variables));
        }

        problem.setConstraints(constraints);
        problem.updateInvolvingConstraints();
        return problem;
    }

    public static enum LearnMethod {
        NONE, EXT, RC, BIN
    }

    public Variable getLastLevelVariable() {
        return levelVariables[currentLevel - 1];
    }

}
