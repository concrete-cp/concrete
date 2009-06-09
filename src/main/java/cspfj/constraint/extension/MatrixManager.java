package cspfj.constraint.extension;

import java.util.Arrays;
import java.util.logging.Logger;

import cspfj.problem.Variable;

public class MatrixManager implements Cloneable {
    private static final Logger LOGGER = Logger.getLogger(MatrixManager.class
            .getName());

    protected final int[] domainSize;

    protected Variable[] variables;

    protected final int arity;

    private Matrix matrix;

    private boolean shared;

    protected int[] tuple;

    private long[][] nbInitConflicts;

    private long[] nbMaxConflicts;

    public MatrixManager(Variable[] scope, Matrix matrix, final boolean shared) {
        super();

        variables = scope;
        domainSize = new int[scope.length];

        for (int i = scope.length; --i >= 0;) {
            domainSize[i] = scope[i].getDomain().maxSize();
        }
        this.arity = scope.length;

        this.matrix = matrix;

        this.shared = shared;

    }

    public void setTuple(final int[] tuple) {
        this.tuple = tuple;
    }

    public void countConflicts() {
        nbInitConflicts = nbConflicts();
        if (nbInitConflicts != null) {
            nbMaxConflicts = new long[arity];
            updateMaxConflicts();
        }
    }

    public boolean set(final int[] tuple, final boolean status) {
        if (matrix.check(tuple) == status) {
            return false;
        }
        if (shared) {
            unshareMatrix();
        }
        matrix.set(tuple, status);
        if (status == false) {
            addConflict(tuple);
        }
        return true;
    }

    public boolean removeTuple(int[] tuple) {
        return set(tuple, false);
    }

    public boolean isTrue(final int[] tuple) {
        return matrix.check(tuple);
    }

    public MatrixManager deepCopy(final Variable[] variables, final int[] tuple)
            throws CloneNotSupportedException {
        final MatrixManager matrix = this.clone();
        matrix.tuple = tuple;
        matrix.variables = variables;
        return matrix;
    }

    public MatrixManager clone() throws CloneNotSupportedException {
        return (MatrixManager) super.clone();
    }

    public boolean check() {
        // checks++;
        return matrix.check(tuple);
    }

    private void firstT() {
        for (int p = variables.length; --p >= 0;) {
            tuple[p] = variables[p].getDomain().maxSize() - 1;
        }
    }

    private boolean nextT() {
        final int[] tuple = this.tuple;

        final Variable[] involvedVariables = this.variables;
        for (int i = arity; --i >= 0;) {
            tuple[i]--;

            if (tuple[i] < 0) {
                tuple[i] = involvedVariables[i].getDomain().maxSize() - 1;
            } else {
                return true;
            }
        }
        return false;
    }

    public void intersect(final Matrix matrix2) {
        firstT();
        do {
            if (!matrix2.check(tuple)) {
                set(tuple, false);
            }
        } while (nextT());

    }

    protected Matrix unshareMatrix() {
        if (!shared) {
            return matrix;
        }
        shared = false;
        return matrix = matrix.clone();
    }

    public String getType() {
        return this.getClass().getSimpleName() + " w/ "
                + matrix.getClass().getSimpleName();
    }

    public Matrix getMatrix() {
        return matrix;
    }

    private void updateMaxConflicts() {
        for (int p = arity; --p >= 0;) {
            long max = 0;
            for (long i : nbInitConflicts[p]) {
                max = Math.max(max, i);
            }
            nbMaxConflicts[p] = max;
        }
    }

    public boolean supportCondition(final int position) {
        return nbMaxConflicts != null
                && getOtherSize(position) > nbMaxConflicts[position];
    }

    private final long getOtherSize(final int position) {
        long size = 1;
        for (int i = arity; --i >= 0;) {
            if (i != position) {
                final int dSize = variables[i].getDomainSize();
                if (size > Long.MAX_VALUE / dSize) {
                    return -1;
                }
                size *= dSize;
            }
        }
        return size;
    }

    public long[][] nbConflicts() {
        final long size = currentSize();
        if (size < 0) {
            return null;
        }

        final long[][] nbInitConflicts = new long[arity][];
        for (int i = arity; --i >= 0;) {
            nbInitConflicts[i] = new long[variables[i].getDomain().maxSize()];
            Arrays.fill(nbInitConflicts[i], 0);
        }

        if (matrix.isEmpty()) {
            return nbInitConflicts;
        }

        LOGGER.fine("Counting " + this + " conflicts");

        if (matrix instanceof TupleSet) {

            final TupleSet tupleSet = (TupleSet) matrix;
            final boolean initialContent = tupleSet.getInitialContent();

            for (int[] tuple : tupleSet) {
                // if (Thread.interrupted()) {
                // throw new InterruptedException();
                // }
                if (initialContent == false) {
                    for (int p = arity; --p >= 0;) {
                        nbInitConflicts[p][tuple[p]]--;
                    }
                } else {
                    for (int p = arity; --p >= 0;) {
                        nbInitConflicts[p][tuple[p]]++;
                    }
                }
            }

            return nbInitConflicts;

        }

        firstT();
        do {
            // if (Thread.interrupted()) {
            // LOGGER.info("Interrupted");
            // throw new InterruptedException();
            // }
            if (!check()) {
                for (int p = arity; --p >= 0;) {
                    nbInitConflicts[p][tuple[p]]++;
                }
            }
        } while (nextT());

        return nbInitConflicts;
    }

    protected final long currentSize() {
        long size = 1;
        for (Variable v : variables) {
            if (size > Integer.MAX_VALUE / v.getDomainSize()) {
                return -1;
            }
            size *= v.getDomainSize();
        }
        return size;
    }

    public void addConflict(int[] tuple) {
        if (nbInitConflicts == null) {
            return;
        }
        for (int p = arity; --p >= 0;) {
            if (nbInitConflicts[p][tuple[p]]++ == nbMaxConflicts[p]) {
                nbMaxConflicts[p]++;
            }
        }
    }
}
