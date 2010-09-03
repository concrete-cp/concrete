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
import java.util.Comparator;
import java.util.HashMap;
import java.util.LinkedHashMap;
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

    private boolean prepared;

    public Problem() {
        super();
        variables = new LinkedHashMap<String, Variable>();
        constraints = new ArrayList<Constraint>();
    }

    public Variable addVariable(final String name, final Domain domain) {
        if (variables.containsKey(name)) {
            throw new IllegalArgumentException("A variable named " + name
                    + " already exists");
        }
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

    private void prepareConstraints() {
        maxArity = Collections.max(constraints, new Comparator<Constraint>() {
            @Override
            public int compare(final Constraint o1, final Constraint o2) {
                return o1.getArity() - o2.getArity();
            }
        }).getArity();

        maxCId = Collections.max(constraints, new Comparator<Constraint>() {
            @Override
            public int compare(final Constraint o1, final Constraint o2) {
                return o1.getId() - o2.getId();
            }
        }).getId();

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

    // private class Semaphore {
    // private int credit = 0;
    //
    // public synchronized void V() {
    // credit++;
    // if (credit <= 0) {
    // notify();
    // }
    // }
    //
    // public synchronized void P() throws InterruptedException {
    // credit--;
    // if (credit < 0) {
    // wait();
    // }
    // }
    //
    // }
    //
    // private class LevelManager extends Thread {
    // private final Semaphore sem;
    // private boolean set;
    // private int startV, endV, startC, endC, level;
    //
    // public LevelManager(Semaphore s) {
    // this.sem = s;
    // }
    //
    // public void run() {
    // for (;;) {
    // try {
    // sem.P();
    // } catch (InterruptedException e) {
    // // TODO Auto-generated catch block
    // e.printStackTrace();
    // }
    //
    // if (set) {
    // for (int i = startV; i < endV; i++) {
    // variableArray[i].setLevel(level);
    // }
    // for (ListIterator<Constraint> itr = constraints
    // .listIterator(startC); itr.nextIndex() < endC;) {
    // itr.next().setLevel(level);
    // }
    // } else {
    // for (int i = startV; i < endV; i++) {
    // variableArray[i].restoreLevel(level);
    // }
    // for (ListIterator<Constraint> itr = constraints
    // .listIterator(startC); itr.nextIndex() < endC;) {
    // itr.next().restore(level);
    // }
    // }
    // sem.V();
    // }
    // }
    //
    // public synchronized void setLevel(int startV, int endV, int startC,
    // int endC, int level) {
    // this.startV = startV;
    // this.endV = endV;
    // this.startC = startC;
    // this.endC = endC;
    // this.level = level;
    // set = true;
    // }
    //
    // public synchronized void restoreLevel(int startV, int endV, int startC,
    // int endC, int level) {
    // this.startV = startV;
    // this.endV = endV;
    // this.startC = startC;
    // this.endC = endC;
    // this.level = level;
    // set = false;
    // }
    // }
    //
    // private final Semaphore sem0 = new Semaphore();
    // private final Semaphore sem1 = new Semaphore();
    // private final LevelManager t0 = new LevelManager(sem0);
    // private final LevelManager t1 = new LevelManager(sem1);
    //
    // private synchronized void setLevel(final int level) {
    // t0.setLevel(0, nbVariables / 2, 0, constraints.size() / 2, level);
    // t1.setLevel(nbVariables / 2, nbVariables, constraints.size() / 2,
    // constraints.size(), level);
    // sem0.V();
    // sem1.V();
    // try {
    // sem0.P();
    // sem1.P();
    // } catch (InterruptedException e) {
    // }
    // }
    //
    // private void restoreLevel(final int level) {
    // t0.restoreLevel(0, nbVariables / 2, 0, constraints.size() / 2, level);
    // t1.restoreLevel(nbVariables / 2, nbVariables, constraints.size() / 2,
    // constraints.size(), level);
    //
    // try {
    // sem.P();
    // sem.P();
    // } catch (InterruptedException e) {
    // throw new IllegalStateException(e);
    // }
    // }

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

    // private void setLevel(final int level) {
    // final Thread t0 = new UpdateLevel(0, variableArray.length / 2, 0,
    // constraints.size() / 2, level);
    // final Thread t1 = new UpdateLevel(variableArray.length / 2,
    // variableArray.length, constraints.size() / 2, constraints
    // .size(), level);
    // t0.start();
    // t1.start();
    //
    // try {
    // t0.join();
    // t1.join();
    // } catch (InterruptedException e) {
    // }
    // }
    //
    // private void restoreLevel(final int level) {
    // final Thread t0 = new RestoreLevel(0, variableArray.length / 2, 0,
    // constraints.size() / 2, level);
    // final Thread t1 = new RestoreLevel(variableArray.length / 2,
    // variableArray.length, constraints.size() / 2, constraints
    // .size(), level);
    // t0.start();
    // t1.start();
    //
    // try {
    // t0.join();
    // t1.join();
    // } catch (InterruptedException e) {
    // }
    // }

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
