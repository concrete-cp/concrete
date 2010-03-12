/*
 * Created on 23 mai 08
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package cspfj.constraint.extension;

import cspfj.constraint.AbstractAC3Constraint;
import cspfj.problem.IntVariable;

public abstract class AbstractExtensionConstraint extends AbstractAC3Constraint
        implements ExtensionConstraint {

    // private static final Logger LOGGER = Logger
    // .getLogger(AbstractExtensionConstraint.class.getName());

    private MatrixManager matrixManager;

    public AbstractExtensionConstraint(MatrixManager matrixManager,
            IntVariable... scope) {
        super(scope);
        this.matrixManager = matrixManager;
        matrixManager.setTuple(tuple);

    }

    public AbstractExtensionConstraint(String name,
            MatrixManager matrixManager, IntVariable... scope) {
        super(name, scope);
        this.matrixManager = matrixManager;
        matrixManager.setTuple(tuple);
    }

    @Override
    public MatrixManager getMatrixManager() {
        return matrixManager;
    }

    @Override
    public final boolean removeTuple(int[] tuple) {
        return matrixManager.removeTuple(tuple);
    }

    @Override
    public boolean check() {
        return matrixManager.check();
    }

    public String getType() {
        return super.getType() + " w/ " + matrixManager.getType();
    }

    @Override
    public final int removeTuples(int[] base) {
        int removed = 0;
        tupleManager.setFirstTuple(base);
        do {
            if (removeTuple(this.tuple)) {
                removed++;
            }
        } while (tupleManager.setNextTuple(base));
        return removed;
    }

    // public final long getNbSupports(final Variable variable, final int index)
    // {
    // return getNbSupports(getPosition(variable), index);
    // }
    //
    // private long getNbSupports(final int position, final int index) {
    // if (nbInitConflicts == null) {
    // return -1;
    // }
    // return (initSize / getVariable(position).getDomain().maxSize())
    // - nbInitConflicts[position][index];
    // }
    //
    // public final long getNbInitConflicts(final int position, final int index)
    // {
    // if (nbInitConflicts == null) {
    // return -1;
    // }
    // return nbInitConflicts[position][index];
    // }
    //
    // public final long getNbMaxConflicts(final int position) {
    // if (nbMaxConflicts == null) {
    // return -1;
    // }
    // return nbMaxConflicts[position];
    // }
    //
    // public long getInitSize() {
    // return initSize;
    // }

    public boolean revise(final int position) {
        if (matrixManager.supportCondition(position)) {

            assert !super.revise(position);
            return false;
        }
        return super.revise(position);
    }

    public int getEvaluation(int reviseCount) {
        int size = 1;
        for (int i = getArity(); --i >= 0;) {
            size *= getVariable(i).getDomainSize();
        }
        return size;
    }
}
