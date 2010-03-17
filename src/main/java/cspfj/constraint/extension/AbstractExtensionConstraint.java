/*
 * Created on 23 mai 08
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package cspfj.constraint.extension;

import java.util.List;
import java.util.ListIterator;

import cspfj.constraint.AbstractAC3Constraint;
import cspfj.constraint.Constraint;
import cspfj.exception.FailedGenerationException;
import cspfj.generator.ConstraintManager;
import cspfj.generator.ExtensionGenerator;
import cspfj.problem.Domain;
import cspfj.problem.Problem;
import cspfj.problem.Variable;
import cspom.constraint.CSPOMConstraint;

public abstract class AbstractExtensionConstraint extends AbstractAC3Constraint
        implements ExtensionConstraint {

    // private static final Logger LOGGER = Logger
    // .getLogger(AbstractExtensionConstraint.class.getName());
    static {
        ConstraintManager.register("ext", AbstractExtensionConstraint.class);
    }

    private MatrixManager matrixManager;

    public AbstractExtensionConstraint(MatrixManager matrixManager,
            Variable... scope) {
        super(scope);
        this.matrixManager = matrixManager;
        matrixManager.setTuple(tuple);

    }

    public AbstractExtensionConstraint(String name,
            MatrixManager matrixManager, Variable... scope) {
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

    public static boolean generate(final CSPOMConstraint constraint,
            final Problem problem) throws FailedGenerationException {

        final Variable[] solverVariables = ConstraintManager
                .getSolverVariables(constraint.getScope(), problem);

        final Domain[] domains = new Domain[constraint.getArity()];
        for (int i = constraint.getArity(); --i >= 0;) {
            final Domain domain = solverVariables[i].getDomain();
            if (domain == null) {
                return false;
            }
            domains[i] = domain;
        }

        final cspom.extension.ExtensionConstraint<Number> extConstraint = (cspom.extension.ExtensionConstraint<Number>) constraint;
        final Matrix matrix = ExtensionGenerator.generate(domains,
                extConstraint.getRelation());

        final Constraint generated;
        if (matrix instanceof Matrix2D) {
            generated = new ExtensionConstraint2D(solverVariables,
                    (Matrix2D) matrix, true);
        } else if (matrix instanceof TupleSet) {
            generated = new ExtensionConstraintDynamic(solverVariables,
                    (TupleSet) matrix, true);
        } else {
            generated = new ExtensionConstraintGeneral(matrix, true,
                    solverVariables);
        }
        problem.addConstraint(generated);
        return true;
    }
}
