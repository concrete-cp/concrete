/*
 * Created on 23 mai 08
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package cspfj.constraint.extension;

import cspfj.constraint.AbstractAC3Constraint;
import cspfj.problem.Variable;

public abstract class AbstractExtensionConstraint extends AbstractAC3Constraint
        implements ExtensionConstraint {

    protected MatrixManager matrix;

    public AbstractExtensionConstraint(MatrixManager matrix, Variable... scope) {
        super(scope);
        this.matrix = matrix;
        matrix.setTuple(tuple);
    }

    public AbstractExtensionConstraint(String name, MatrixManager matrix,
            Variable... scope) {
        super(name, scope);
        this.matrix = matrix;
        matrix.setTuple(tuple);
    }

    @Override
    public MatrixManager getMatrix() {
        return matrix;
    }

    @Override
    public boolean removeTuple(int[] tuple) {
        if (matrix.removeTuple(tuple)) {
            addConflict(tuple);
            return true;
        }
        return false;
    }

    @Override
    public boolean check() {
        return matrix.check();
    }

    public String getType() {
        return super.getType() + " w/ " + matrix.getType();
    }

    @Override
    public int removeTuples(int[] base) {
        int removed = 0;
        tupleManager.setFirstTuple(base);
        do {
            if (removeTuple(this.tuple)) {
                removed++;
            }
        } while (tupleManager.setNextTuple(base));
        return removed;
    }
}
