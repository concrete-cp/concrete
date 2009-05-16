package cspfj.constraint;

import java.util.Collection;
import java.util.Set;

import cspfj.filter.RevisionHandler;
import cspfj.problem.Variable;

public interface Constraint {
    int getArity();

    boolean isInvolved(final Variable variable);

    Variable getVariable(final int position);

    boolean checkFirst();

    int getId();

    Variable[] getScope();

    Set<Variable> getScopeSet();

    int getPositionInVariable(final int variablePosition);

    int getPosition(final Variable variable);

    boolean checkFirstWith(final int variablePos, final int index);

    boolean isActive();

    void setActive(final boolean b);

    void setPositionInVariable(final int variablePosition,
            final int constraintPosition);

    Constraint deepCopy(final Collection<Variable> variables)
            throws CloneNotSupportedException;

    void initNbSupports() throws InterruptedException;

    long getNbSupports(final Variable variable, final int index);

    // boolean skipRevision(int i);

    /**
     * The constraint propagator
     * 
     * @param revisator
     * @return false iff an inconsistency has been detected
     */
    boolean revise(final RevisionHandler revisator);

    // int getOtherSize(int position);

    long getNbMaxConflicts(final int position);

    long getNbInitConflicts(final int position, final int index);

    boolean isBound(Variable variable);

    String getType();

    boolean check();

    void setLevel(int level);

    void restore(int level);

    long getInitSize();

    int getWeight();

    void incWeight();

    void setWeight(final int weight);

    boolean getRemovals(int position);

    void setRemovals(int position, boolean value);

    void fillRemovals(final boolean value);

    boolean hasNoRemovals();

    // boolean findValidTuple(int position, int index);
}