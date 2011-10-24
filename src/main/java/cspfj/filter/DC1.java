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

import java.util.Map;
import java.util.logging.Logger;

import scala.collection.IndexedSeq;
import cspfj.ParameterManager;
import cspfj.StatisticsManager;
import cspfj.problem.LearnMethod;
import cspfj.problem.NoGoodLearner;
import cspfj.problem.Problem;
import cspfj.problem.Variable;
import cspfj.util.Parameter;
import cspfj.util.Statistic;

/**
 * @author Julien VION
 * 
 */
public final class DC1 extends AbstractSAC {

    private static final Logger LOGGER = Logger.getLogger(DC1.class.getName());

    @Parameter("dc1.addConstraints")
    private static LearnMethod addConstraints = LearnMethod.CONSERVATIVE;

    static {
        ParameterManager.register(DC1.class);
        StatisticsManager.register(DC1.class);
    }

    @Statistic
    private static int addedConstraints = 0;

    @Statistic
    private static int nbNoGoods;

    private final NoGoodLearner ngl;

    public DC1(final Problem problem) {
        super(problem, new AC3(problem));
        ngl = new NoGoodLearner(problem, addConstraints);
    }

    @Override
    protected boolean reduce() throws InterruptedException {
        final int nbC = problem.constraints().size();
        // ExtensionConstraintDynamic.quick = true;
        final boolean result;
        try {
            result = super.reduce();
        } finally {
            addedConstraints += problem.constraints().size() - nbC;
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
        final IndexedSeq<Variable> variables = problem.variables();

        int mark = 0;

        int v = 0;

        do {
            final Variable variable = variables.apply(v);
            // if (logger.isLoggable(Level.FINE)) {
            LOGGER.info(variable.toString());
            // }
            if (variable.dom().size() > 1 && singletonTest(variable)) {
                return false;
            }
            if (++v >= variables.size()) {
                v = 0;
            }
        } while (v != mark);

        return true;

    }

    protected boolean singletonTest(final Variable variable)
            throws InterruptedException {
        boolean changedGraph = false;
        for (int index = variable.dom().first(); index >= 0; index = variable
                .dom().next(index)) {
            if (Thread.interrupted()) {
                throw new InterruptedException();
            }
            if (!variable.dom().present(index)) {
                continue;
            }

            // if (logger.isLoggable(Level.FINER)) {
            // LOGGER.fine(variable + " <- " + variable.getDomain().value(index)
            // + "(" + index + ")");
            // }

            problem.push();
            variable.dom().setSingle(index);

            nbSingletonTests++;

            if (filter.reduceAfter(variable)) {

                // final Map<Variable[], List<int[]>> noGoods =
                // problem.noGoods();
                changedGraph = !ngl.binNoGoods(variable).isEmpty()
                        || changedGraph;
                // logger.info(noGoods.toString());

                problem.pop();

                // changedGraph = problem.noGoodsToConstraints(noGoods,
                // addConstraints);
            } else {
                problem.pop();
                LOGGER.fine("Removing " + variable + ", " + index);

                variable.dom().remove(index);
                changedGraph = true;
            }
        }
        return changedGraph;
    }

    public Map<String, Object> getStatistics() {
        final Map<String, Object> statistics = super.getStatistics();
        statistics.put("CDC-nogoods", nbNoGoods);
        statistics.put("CDC-added-constraints", addedConstraints);
        return statistics;
    }

    public String toString() {
        return "DC w/ " + filter + " L " + ngl.learnMethod();
    }
}
