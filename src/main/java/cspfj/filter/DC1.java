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

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.logging.Logger;

import cspfj.AbstractSolver;
import cspfj.constraint.Constraint;
import cspfj.constraint.DynamicConstraint;
import cspfj.problem.NoGoodLearner;
import cspfj.problem.Problem;
import cspfj.problem.Variable;
import cspfj.problem.NoGoodLearner.LearnMethod;
import cspfj.util.BitVector;

/**
 * @author Julien VION
 * 
 */
public final class DC1 extends AbstractSAC {

    private static final Logger LOGGER = Logger.getLogger(DC1.class.getName());

    private static final LearnMethod LEARN_METHOD;

    static {
        final String lm = AbstractSolver.PARAMETERS.get("dc.addConstraints");
        if (lm == null) {
            LEARN_METHOD = LearnMethod.NONE;
        } else {
            LEARN_METHOD = LearnMethod.valueOf(lm);
        }
    }

    private int addedConstraints = 0;

    private final Variable[] variables;

    private int nbNoGoods;

    private final NoGoodLearner ngl;

    public DC1(final Problem problem) {
        super(problem, new AC3(problem));
        this.variables = problem.getVariables();
        ngl = new NoGoodLearner(problem, LEARN_METHOD);
    }

    @Override
    protected boolean reduce() throws InterruptedException {
        final int nbC = problem.getNbConstraints();
        // ExtensionConstraintDynamic.quick = true;
        final boolean result;
        try {
            result = super.reduce();
        } finally {
            addedConstraints += problem.getNbConstraints() - nbC;
        }
        // ExtensionConstraintDynamic.quick = false;
        // for (Constraint c : problem.getConstraints()) {
        // if (c instanceof RCConstraint) {
        // ((RCConstraint) c).flushPending();
        // }
        // }
        return result;
    }
    
    public boolean control() throws InterruptedException {
        final Filter filter = this.filter;

        if (!filter.reduceAll()) {
            return false;
        }
        final Variable[] variables = problem.getVariables();

        int mark = 0;

        int v = 0;

        do {
            final Variable variable = variables[v];
            // if (logger.isLoggable(Level.FINE)) {
            LOGGER.info(variable.toString());
            // }
            if (variable.getDomainSize() > 1 && singletonTest(variable)) {
            	return false;
            }
            if (++v >= variables.length) {
                v = 0;
            }
        } while (v != mark);

        return true;

    }

    protected boolean singletonTest(final Variable variable)
            throws InterruptedException {
        boolean changedGraph = false;
        for (int index = variable.getFirst(); index >= 0; index = variable
                .getNext(index)) {
            if (Thread.interrupted()) {
                throw new InterruptedException();
            }
            if (!variable.isPresent(index)) {
                continue;
            }

            // if (logger.isLoggable(Level.FINER)) {
            LOGGER.fine(variable + " <- " + variable.getDomain().value(index)
                    + "(" + index + ")");
            // }

            problem.push();
            variable.setSingle(index);

            nbSingletonTests++;

            if (filter.reduceAfter(variable)) {

                // final Map<Variable[], List<int[]>> noGoods =
                // problem.noGoods();
                changedGraph = noGoods(variable) | changedGraph;
                // logger.info(noGoods.toString());

                problem.pop();

                // changedGraph = problem.noGoodsToConstraints(noGoods,
                // addConstraints);
            } else {
                problem.pop();
                LOGGER.fine("Removing " + variable + ", " + index);

                variable.remove(index);
                changedGraph = true;
            }
        }
        return changedGraph;
    }

    public boolean noGoods(final Variable firstVariable) {
        List<Integer> tuple = new ArrayList<Integer>(2);

        final Set<Variable> scopeSet = new HashSet<Variable>(2);

        scopeSet.add(firstVariable);
        tuple.add(firstVariable.getFirst());
        final Variable[] scopeArray = new Variable[] { firstVariable, null };

        boolean modified = false;
        final Collection<Constraint> addedConstraints = new ArrayList<Constraint>();

        for (Variable fv : variables) {

            // logger.fine("checking " +
            // getVariable(levelVariables[level-1]));

            if (fv == firstVariable) {
                continue;
            }

            final BitVector changes = fv.getDomain().getAtLevel(0).xor(
                    fv.getDomain().getAtLevel(1));
            if (changes.isEmpty()) {
                continue;
            }

            scopeSet.add(fv);
            final DynamicConstraint constraint = ngl.learnConstraint(scopeSet);
            scopeSet.remove(fv);

            if (constraint == null) {
                continue;
            }

            scopeArray[1] = fv;

            final int[] base = new int[constraint.getArity()];
            final int varPos = NoGoodLearner.makeBase(scopeArray, tuple,
                    constraint, base);

            int newNogoods = 0;
            for (int i = changes.nextSetBit(0); i >= 0; i = changes
                    .nextSetBit(i + 1)) {
                base[varPos] = i;
                newNogoods += constraint.removeTuples(base);

            }
            if (newNogoods > 0) {
                nbNoGoods += newNogoods;
                modified = true;
                if (constraint.getId() > problem.getMaxCId()) {
                    LOGGER.info("Added " + constraint);
                    addedConstraints.add(constraint);
                }
            }
        }

        if (modified) {
            LOGGER.fine(nbNoGoods + " nogoods");

            if (!addedConstraints.isEmpty()) {
                for (Constraint c : addedConstraints) {
                    problem.addConstraint(c);
                }

                problem.prepare();
                LOGGER.info(problem.getNbConstraints() + " constraints");
            }
        }
        return modified;
    }

    public Map<String, Object> getStatistics() {
        final Map<String, Object> statistics = super.getStatistics();
        statistics.put("CDC-nogoods", nbNoGoods);
        statistics.put("CDC-added-constraints", addedConstraints);
        return statistics;
    }

    public String toString() {
        return "DC w/ " + filter + " L " + LEARN_METHOD;
    }
}
