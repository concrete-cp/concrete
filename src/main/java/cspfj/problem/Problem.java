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
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

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

	public Problem() {
		super();
		variables = new HashMap<String, Variable>();
		constraints = new ArrayList<Constraint>();
	}

	public Variable addVariable(final String name, final Domain domain) {
		if (variables.containsKey(name)) {
			throw new IllegalArgumentException("A variable named " + name
					+ " already exists");
		}
		final Variable var = new Variable(name, domain);
		variables.put(name, var);
		return var;
	}

	public void addConstraint(final Constraint constraint) {
		constraints.add(constraint);
	}

	public void prepareVariables() {
		maxDomainSize = 0;
		maxVId = 0;

		variableArray = variables.values().toArray(
				new Variable[variables.size()]);

		for (Variable var : variableArray) {
			maxDomainSize = Math.max(maxDomainSize, var.getDomain().maxSize());
			maxVId = Math.max(maxVId, var.getId());
		}

		nbVariables = variables.size();
	}

	public void prepareConstraints() {
		maxArity = Collections.max(constraints, new Comparator<Constraint>() {
			@Override
			public int compare(final Constraint o1, final Constraint o2) {
				return o1.getArity() - o2.getArity();
			}
		}).getArity();

		final Map<Integer, List<Constraint>> invConstraints = new HashMap<Integer, List<Constraint>>(
				variableArray.length);

		for (Variable v : getVariables()) {
			invConstraints.put(v.getId(), new ArrayList<Constraint>());
		}

		for (Constraint c : getConstraints()) {
			for (Variable v : c.getScope()) {
				invConstraints.get(v.getId()).add(c);
			}
		}

		for (Variable v : getVariables()) {
			final Collection<Constraint> involvingConstraints = invConstraints
					.get(v.getId());
			v.setInvolvingConstraints(involvingConstraints
					.toArray(new Constraint[involvingConstraints.size()]));
		}
	}

	public int getNbVariables() {
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
		// currentLevel = level;
		for (Variable v : variableArray) {
			v.setLevel(level);
		}
		for (Constraint c : constraints) {
			c.setLevel(level);
		}
	}

	private void restoreLevel(final int level) {
		// currentLevel = level;
		for (Variable v : variableArray) {
			v.restoreLevel(level);
		}
		for (Constraint c : getConstraints()) {
			c.restore(level);
		}
	}

	public void reset() {
		currentLevel = 0;
		for (Variable v : variableArray) {
			v.reset();
		}
		for (Constraint c : getConstraints()) {
			c.restore(0);
		}
	}

	@Override
	public String toString() {
		final StringBuilder sb = new StringBuilder();
		for (Variable v : variables.values()) {
			sb.append(v).append('\n');
		}

		int entailed = 0;
		for (Constraint c : constraints) {
			if (c.isEntailed()) {
				entailed++;
			} else {
				sb.append(c.toString()).append('\n');
			}
		}
		sb.append("Total ").append(getNbVariables()).append(" variables, ")
				.append(getNbConstraints() - entailed).append(
						" constraints and ").append(entailed).append(
						" entailed constraints");

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

	public Variable getVariable(final String name) {
		return variables.get(name);
	}

}
