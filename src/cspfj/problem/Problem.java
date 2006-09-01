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
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.logging.Level;
import java.util.logging.Logger;

import cspfj.constraint.Constraint;
import cspfj.constraint.Constraint;
import cspfj.exception.FailedGenerationException;

public final class Problem {
    private Variable[] variables;

    private Map<Integer, Constraint> constraints;

    private int nbFutureVariables;

    private final static Logger logger = Logger.getLogger("cspfj.Problem");

    private int maxDomainSize;

    private int[] levelVariables;

    final private static ArrayList<Variable> scope = new ArrayList<Variable>();

    final private static ArrayList<Integer> tuple = new ArrayList<Integer>();

    private boolean[] isPastVariable;

    private int maxArity;

    public Problem() {
        super();
    }

    public Problem(Variable[] variables, Constraint[] constraints) {
        this();
        setVariables(variables);
        setConstraints(constraints);
        updateInvolvingConstraints();
    }

    public static Problem load(final ProblemGenerator generator)
            throws FailedGenerationException {
        final Problem problem = new Problem();
        Variable.resetVId();
        Constraint.resetCId();

        logger.fine("Generating");
        generator.generate();

        logger.fine("Setting Variables");
        problem.setVariables(generator.getVariables());
        logger.fine("Setting Constraints");
        problem.setConstraints(generator.getConstraints());

        logger.fine("Updating InvolvingConstraints");
        problem.updateInvolvingConstraints();

        // for (Constraint c :
        // generator.getVariables()[21].getInvolvingConstraints()) {
        // if (c.getInvolvedVariables()[1] == generator.getVariables()[23])
        // System.out.println(c);
        // }

        logger.fine("Done");
        return problem;

    }

    public int getNbFutureVariables() {
        return nbFutureVariables;
    }

    private void setVariables(final Variable[] vars) {

        this.variables = new Variable[vars.length];

        maxDomainSize = 0;

        for (Variable var : vars) {
            variables[var.getId()] = var;
            if (var.getDomain().length > maxDomainSize) {
                maxDomainSize = var.getDomain().length;
            }
        }

        nbFutureVariables = vars.length;

        levelVariables = new int[getNbVariables()];
        for (int i = 0; i < getNbVariables(); i++) {
            levelVariables[i] = -1;
        }

        isPastVariable = new boolean[getNbVariables()];

    }

    private void setConstraints(final Constraint[] cons) {
        this.constraints = new HashMap<Integer, Constraint>(cons.length);
        for (Constraint c : cons) {
            this.constraints.put(c.getId(), c);
            if (c.getArity() > maxArity) {
                maxArity += c.getArity();
            }
        }

        // resetConstraint = new boolean[constraints.length];
    }

    private void updateInvolvingConstraints() {
        final List<List<Constraint>> invConstraints = new ArrayList<List<Constraint>>(
                variables.length);

        for (Variable v : variables) {
            invConstraints.add(v.getId(), new ArrayList<Constraint>());
        }

        for (Constraint c : getConstraints()) {
            for (Variable v : c.getInvolvedVariables()) {
                invConstraints.get(v.getId()).add(c);
            }
        }

        for (Variable v : variables) {

            v.setInvolvingConstraints(invConstraints.get(v.getId()).toArray(
                    new Constraint[invConstraints.get(v.getId()).size()]));
        }

        // setValueOrders() ;
    }

    public int getNbVariables() {
        return variables.length;
    }

    public int getNbConstraints() {
        return constraints.size();
    }

    public Variable[] getVariables() {
        return variables;
    }

    public Variable getVariable(final int vId) {
        return variables[vId];
    }

    public Collection<Constraint> getConstraints() {
        return constraints.values();
    }

    public Collection<Integer> getConstraintIDs() {
        return constraints.keySet();
    }

    public Constraint getConstraint(final int c) {
        return constraints.get(c);
    }

    public void increaseFutureVariables() {
        nbFutureVariables++;
    }

    public void decreaseFutureVariables() {
        nbFutureVariables--;
    }

    public void restore(final int level) {
        // for (int i = 0; i < resetConstraint.length; i++) {
        // resetConstraint[i] = false;
        // }
        for (Variable v : variables) {
            if (!v.isAssigned()) {
                v.restoreLevel(level);
            }

            // if (v.restore(level)) {
            // for (AbstractConstraint c : v.getInvolvingConstraints()) {
            // if (!resetConstraint[c.getId()]) {
            // c.initLast();
            // resetConstraint[c.getId()] = true;
            // }
            // }
            // }
        }

    }

    public void restoreAll() {
        for (Variable v : variables) {
            if (v.isAssigned()) {
                v.unassign(this);
            }

            v.restoreLevel(1);

        }
        // for (AbstractConstraint c : constraints) {
        // c.initLast();
        // }
    }

    public void setValueOrders(final Random random) {
        // logger.info("Initializing value orders");
        for (Variable v : variables) {
            v.orderIndexes(random);
        }

    }

    @Override
    public String toString() {
        final StringBuffer sb = new StringBuffer();
        for (Variable v : variables) {
            sb.append(v.toString());
            sb.append(" : ");
            sb.append(Arrays.toString(v.getCurrentDomain()));
            sb.append('\n');
        }
        for (Constraint c : getConstraints()) {
            sb.append(c.toString());
            sb.append('\n');
        }

        return sb.toString();
    }

    public int getMaxDomainSize() {
        return maxDomainSize;
    }

    public boolean addNoGood() {

        Constraint constraint = null;

        final List<Variable> scope = Problem.scope;

        for (Constraint c : scope.get(0).getInvolvingConstraints()) {
            if (c.getArity() != scope.size()) {
                continue;
            }
            boolean valid = true;
            for (Variable variable : scope) {
                if (!c.isInvolved(variable)) {
                    valid = false;
                    break;
                }
            }

            if (valid) {
                constraint = c;
                break;
            }
        }

        if (constraint == null) {
            return false;
            // constraint = new ExtensionConstraint(scope
            // .toArray(new Variable[scope.size()]));
            // final Constraint[] newConstraints = new
            // Constraint[constraints.length + 1];
            // System.arraycopy(constraints, 0, newConstraints, 1,
            // constraints.length);
            // newConstraints[0] = constraint;
            // constraints = newConstraints;
            // updateInvolvingConstraints();
            // logger.fine("Creating constraint " + constraint + " ("
            // + constraints.length + " constraints)");
        }

        return constraint.removeTuple(scope, tuple);
    }

    public int addNoGoods() {
        int nbNoGoods = 0;

        final int[] levelVariables = this.levelVariables;

        if (levelVariables[0] < 0) {
            return 0;
        }

        final List<Variable> scope = Problem.scope;
        final List<Integer> tuple = Problem.tuple;
        final boolean[] isPastVariable = this.isPastVariable;

        Arrays.fill(isPastVariable, false);
        // scope.clear();
        tuple.clear();

        scope.add(0, variables[levelVariables[0]]);
        tuple.add(0, variables[levelVariables[0]].getFirstPresentIndex());

        for (int level = 1; level < levelVariables.length; level++) {

            isPastVariable[levelVariables[level - 1]] = true;

            scope.add(level, null);
            tuple.add(level, null);

            for (Variable fv : variables) {
                if (isPastVariable[fv.getId()]) {
                    continue;
                }

                scope.set(level, fv);
                // logger.fine(fv.toString()) ;
                for (int lostIndex = fv.getLastAbsent(); lostIndex >= 0; lostIndex = fv
                        .getPrevAbsent(lostIndex)) {
                    if (fv.getRemovedLevel(lostIndex) == level) {
                        tuple.set(level, lostIndex);

                        if (addNoGood()) {
                            nbNoGoods++;
                        }
                    }

                }

            }

            if (levelVariables[level] >= 0) {
                scope.set(level, variables[levelVariables[level]]);
                tuple.set(level, variables[levelVariables[level]]
                        .getFirstPresentIndex());
            } else {
                break;
            }
        }
        if (logger.isLoggable(Level.FINE)) {
            logger.fine(nbNoGoods + " nogoods");
        }
        scope.clear();
        return nbNoGoods;
    }

    public void setLevelVariables(final int level, final int i) {
        levelVariables[level] = i;

    }

    public final int getMaxArity() {
        return maxArity;
    }

    public void restoreValues() {

        for (Variable v : variables) {
            if (v.isAssigned()) {
                v.unassign(this);
            }
            v.restoreLevel(-1);
        }

    }

}
