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
import java.util.Collection;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import com.google.common.base.Function;
import com.google.common.base.Joiner;
import com.google.common.base.Preconditions;
import com.google.common.collect.ImmutableMultimap;
import com.google.common.collect.Iterables;
import com.google.common.collect.Lists;
import com.google.common.collect.Multimap;
import com.google.common.collect.Ordering;

import cspfj.constraint.Constraint;

public final class Problem {
    private Map<String, Variable> variables;

    private Variable[] variableArray;

    private int nbVariables = 0;

    private List<Constraint> constraints;

    private int maxDomainSize;

    private int maxArity;

    private int maxVId;

    private int maxCId;

    private int currentLevel;

    private boolean prepared;

    public Problem() {
        super();
        variables = new LinkedHashMap<String, Variable>();
        constraints = new ArrayList<Constraint>();
    }

    public Variable addVariable(final String name, final Domain domain) {
        Preconditions.checkArgument(!variables.containsKey(name),
                "A variable named %s already exists", name);

        final Variable var = new Variable(name, domain);
        variables.put(name, var);
        prepared = false;
        return var;
    }

    public void addConstraint(final Constraint constraint) {
        constraints.add(constraint);
        prepared = false;
    }

    public void prepare() {
        if (!prepared) {
            prepareVariables();
            prepareConstraints();
            prepared = true;
        }
    }

    private void prepareVariables() {
        maxDomainSize = Ordering.natural().max(
                Iterables.transform(variables.values(),
                        new Function<Variable, Integer>() {
                            @Override
                            public Integer apply(Variable input) {
                                return input.getDomain().maxSize();
                            }
                        }));

        maxVId = Ordering.natural().max(
                Iterables.transform(variables.values(),
                        new Function<Variable, Integer>() {
                            @Override
                            public Integer apply(Variable input) {
                                return input.getId();
                            }
                        }));

        variableArray = variables.values().toArray(
                new Variable[variables.size()]);

        nbVariables = variables.size();
    }

    private void prepareConstraints() {
        if (constraints.isEmpty()) {
            maxArity = 0;
            maxCId = -1;
            final List<Constraint> empty = Collections.emptyList();
            for (Variable v : getVariables()) {
                v.setInvolvingConstraints(empty);
            }
        } else {
            maxArity = Collections.max(Lists.transform(constraints,
                    new Function<Constraint, Integer>() {
                        @Override
                        public Integer apply(Constraint input) {
                            return input.getArity();
                        }
                    }));

            maxCId = Collections.max(Lists.transform(constraints,
                    new Function<Constraint, Integer>() {
                        @Override
                        public Integer apply(Constraint input) {
                            return input.getId();
                        }
                    }));

            final ImmutableMultimap.Builder<Variable, Constraint> builder = ImmutableMultimap
                    .builder();

            for (Constraint c : getConstraints()) {
                for (Variable v : c.getScope()) {
                    builder.put(v, c);
                }
            }

            final Multimap<Variable, Constraint> invConstraints = builder
                    .build();

            for (Variable v : getVariables()) {
                final Collection<Constraint> involvingConstraints = invConstraints
                        .get(v);
                v.setInvolvingConstraints(involvingConstraints);
            }
        }
    }

    public int getNbVariables() {
        prepare();
        return nbVariables;
    }

    public int getNbConstraints() {
        return constraints.size();
    }

    public List<Constraint> getConstraints() {
        return constraints;
    }

    public Variable[] getVariables() {
        return variableArray;
    }

    public void push() {
        currentLevel++;
        setLevel(currentLevel);

    }

    public void pop() {
        currentLevel--;
        restoreLevel(currentLevel);

    }

    private void setLevel(final int level) {
        for (Variable v : variableArray) {
            v.setLevel(level);
        }
        for (Constraint c : constraints) {
            c.setLevel(level);
        }
    }

    private void restoreLevel(final int level) {
        for (Variable v : variableArray) {
            v.restoreLevel(level);
        }
        for (Constraint c : constraints) {
            c.restore(level);
        }
    }

    public void reset() {
        if (currentLevel > 0) {
            currentLevel = 0;
            for (Variable v : variableArray) {
                v.reset();
            }
            for (Constraint c : getConstraints()) {
                c.restore(0);
            }
        }
    }

    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder();
        Joiner.on('\n').appendTo(sb, variables.values());
        sb.append('\n');
        int entailed = 0;
        for (Constraint c : constraints) {
            if (c.isEntailed()) {
                entailed++;
            } else {
                sb.append(c.toString()).append('\n');
            }
        }
        sb.append("Total ").append(getNbVariables()).append(" variables, ")
                .append(getNbConstraints() - entailed)
                .append(" constraints and ").append(entailed)
                .append(" entailed constraints");

        return sb.toString();
    }

    public int getMaxDomainSize() {
        return maxDomainSize;
    }

    public int getCurrentLevel() {
        return currentLevel;
    }

    public int getMaxArity() {
        return maxArity;
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

    public Variable getVariable(final String name) {
        return variables.get(name);
    }

}
