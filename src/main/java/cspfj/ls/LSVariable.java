/*
 * Created on 16 mars 2007
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package cspfj.ls;

import java.util.Arrays;
import java.util.Map;

import cspfj.constraint.Constraint;
import cspfj.problem.Variable;
import cspfj.util.TieManager;

public final class LSVariable {

    private final Variable variable;

    private final int vid;

    private int bestIndex;

    private final TieManager tieManager;

    private final int[] nbConflicts;

    private final LSConstraint[] constraints;

    private final boolean[][] check;

    private int currentConflicts;

    private int assignedIndex;

    // private final static Logger logger =
    // Logger.getLogger("ConflictsManager");

    private boolean critic;

    public final boolean[] criticConstraints;

    // private final LocalSolver solver;

    public LSVariable(final Variable variable, final TieManager tieManager) {
        super();
        this.variable = variable;
        vid = variable.getId();
        this.constraints = new LSConstraint[variable.getInvolvingConstraints().length];
        this.tieManager = tieManager;
        nbConflicts = new int[variable.getDomain().maxSize()];

        check = new boolean[constraints.length][variable.getDomain().maxSize()];
        criticConstraints = new boolean[constraints.length];
        assignedIndex = -1;
        currentConflicts = -1;
    }

    public void setLSConstraints(Map<Constraint, LSConstraint> lsConstraints) {
        for (int i = constraints.length; --i >= 0;) {
            constraints[i] = lsConstraints.get(variable
                    .getInvolvingConstraints()[i]);
        }
    }

    public Variable getVariable() {
        return variable;
    }

    public int getBestIndex() {
        return bestIndex;
    }

    public int getBestImprovment() {
        return getImprovment(bestIndex);
    }

    public int getImprovment(final int index) {
        assert currentConflicts >= 0;
        return nbConflicts[index] - currentConflicts;
    }

    public void assign(int index) {
        assignedIndex = index;
    }

    public void reAssign(int index) {
        assignedIndex = index;

        for (LSConstraint c : constraints) {
            final LSVariable[] involvedVariables = c.getScope();
            for (int n = involvedVariables.length; --n >= 0;) {
                final LSVariable neighbour = involvedVariables[n];
                if (neighbour != this)
                neighbour.update(c, n);
            }
        }

        initCritic();
    }

    public void assignBestInitialIndex() {

        final TieManager tieManager = this.tieManager;
        final LSConstraint[] constraints = this.constraints;

        tieManager.clear();

        final Variable variable = this.variable;

        for (int i = variable.getFirst(); i >= 0; i = variable.getNext(i)) {
            assignedIndex = i;

            int indexConflicts = 0;

            for (int c = constraints.length; --c >= 0;) {
                final LSConstraint constraint = constraints[c];

                if (!skipCount(constraint) && !constraint.check()) {
                    // logger.warning("No tuple found");
                    indexConflicts++;// = solver.getWeight(constraint);
                }
            }

            tieManager.newValue(i, indexConflicts);

        }

        // variable.resetAssign();

        bestIndex = assignedIndex = tieManager.getBestValue();

        // variable.firstAssign(bestIndex);
    }

    private boolean skipCount(LSConstraint constraint) {
        for (LSVariable v : constraint.getScope()) {
            if (v != this && v.getAssignedIndex() < 0) {
                return true;
            }

        }
        return false;
    }

    public void initNbConflicts() {
        final int[] nbConflicts = this.nbConflicts;
        Arrays.fill(nbConflicts, 0);
        final LSConstraint[] constraints = this.constraints;
        final Variable variable = this.variable;

        for (int c = constraints.length; --c >= 0;) {
            final boolean[] check = this.check[c];
            final LSConstraint constraint = constraints[c];
            final int position = constraint.getPosition(variable);
            for (int i = variable.getFirst(); i >= 0; i = variable.getNext(i)) {

                // if (variable.getRemovedLevel(i) >= 0) {
                // continue;
                // }

                check[i] = constraint.checkWith(position, i);
                if (!check[i]) {
                    nbConflicts[i]++;// = solver.getWeight(constraint);
                }
            }
        }
        currentConflicts = nbConflicts[assignedIndex];
        initCritic();
    }

    private void initCritic() {
        critic = false;
        final boolean[] criticConstraints = this.criticConstraints;
        for (int c = constraints.length; --c >= 0;) {
            if (criticConstraints[c] = critic(c)) {
                critic = true;
                assert currentConflicts > 0;
                break;
            }
        }
    }

    private boolean critic(final int constraintPos) {
        final boolean[] check = this.check[constraintPos];
        final boolean currentCheck = check[assignedIndex];
        if (currentCheck) {
            return false;
        }
        for (int i = variable.getLast() + 1; --i >= 0;) {
            if (variable.isPresent(i) && check[i]) {
                assert currentConflicts > 0;
                return true;
            }
        }
        return false;
    }

    public int getCurrentConflicts() {
        return currentConflicts;
    }

    public void update(final LSConstraint c, final int variablePos) {
        final TieManager tieManager = this.tieManager;
        tieManager.clear();
        final int[] nbConflicts = this.nbConflicts;

        assert c.getScope()[variablePos] == this;

        final Variable variable = this.variable;

        // final int position = constraint.getPosition(variable);

        final int constraintPos = c.getPositionInVariable(variablePos);

        assert getLSConstraints()[constraintPos] == c;

        for (int i = variable.getLast() + 1; --i >= 0;) {
            //13%
            if (!variable.isPresent(i)) {
                continue;
            }

            //30%
            final boolean check = c.checkWith(variablePos, i);
            
            
            if (check != this.check[constraintPos][i]) {
                if (check) {
                    nbConflicts[i]--;// = solver.getWeight(c);
                } else {
                    nbConflicts[i]++;// = solver.getWeight(c);
                }
                this.check[constraintPos][i] ^= true;
            }

            //22%
            tieManager.newValue(i, nbConflicts[i]);
        }
        bestIndex = tieManager.getBestValue();
        currentConflicts = nbConflicts[assignedIndex];

        // 5%
        initCritic();
    }

    // private void updateCritic(int constraintPos) {
    // final boolean wasCritic = criticConstraints[constraintPos];
    // assert wasCritic ? critic : true;
    // criticConstraints[constraintPos] = critic(constraintPos);
    // if (criticConstraints[constraintPos]) {
    // critic = true;
    // } else if (wasCritic) {
    // critic = false;
    // for (int c = criticConstraints.length; --c >= 0;) {
    // if (criticConstraints[c]) {
    // critic = true;
    // break;
    // }
    // }
    // }
    //
    // }

    // public boolean updateAfterIncrement(final Constraint c, final int pos) {
    //
    // final TieManager tieManager = this.tieManager;
    // tieManager.clear();
    //
    // final int[] nbConflicts = this.nbConflicts;
    // final Variable variable = this.variable;
    // assert c.getPosition(variable) == pos;
    // assert variable.getInvolvingConstraints()[c.getPositionInVariable(pos)]
    // == c;
    //
    // final boolean[] check = this.check[c.getPositionInVariable(pos)];
    // assert !check[assignedIndex];
    // for (int i = domain.length; --i >= 0;) {
    // if (variable.getRemovedLevel(i) >= 0) {
    // continue;
    // }
    // if (!check[i]) {
    // nbConflicts[i]++;
    // }
    // tieManager.newValue(i, nbConflicts[i]);
    // }
    // currentConflicts = nbConflicts[assignedIndex];
    // if (bestIndex == tieManager.getBestValue()) {
    // return false;
    // }
    // bestIndex = tieManager.getBestValue();
    // return true;
    // }

    public int getBestIndex(final TabuManager tabuManager,
            final int aspiration, final int nbIt) {
        final int limit = currentConflicts - aspiration;
        final int vid = this.vid;

        if (!tabuManager.isTabu(vid, bestIndex, nbIt)
                || nbConflicts[bestIndex] < limit) {
            return bestIndex;
        }

        final TieManager tieManager = this.tieManager;
        tieManager.clear();

        final int[] nbConflicts = this.nbConflicts;
        final Variable variable = this.variable;

        for (int i = variable.getFirst(); i >= 0; i = variable.getNext(i)) {
            if (!tabuManager.isTabu(vid, i, nbIt) || nbConflicts[i] < limit) {
                tieManager.newValue(i, nbConflicts[i]);
            }
        }

        return tieManager.getBestValue();
    }

    // public void reAssign(final int index) {
    // variable.reAssign(index);
    // assignedIndex = index;
    // currentConflicts = nbConflicts[index];
    // initCritic();
    // }

    public LSConstraint[] getLSConstraints() {
        return constraints;
    }

    public int getAssignedIndex() {
        return assignedIndex;
    }

    public String toString() {
        return "ls-" + variable.toString();
    }

    public boolean isCritic() {
        return critic;
    }
    //
    // public void shuffleBest() {
    // final TieManager tieManager = this.tieManager;
    // tieManager.clear();
    // for (int i = domain.length; --i >= 0;) {
    // if (variable.getRemovedLevel(i) < 0) {
    // tieManager.newValue(i, nbConflicts[i]);
    // }
    // }
    //
    // bestIndex = tieManager.getBestValue();
    // }
}
