package cspfj.constraint.extension;

import java.util.Arrays;

import cspfj.problem.Variable;

public abstract class AbstractMatrixManager implements MatrixManager {
    private final int[] domainSize;

    
    protected Variable[] variables;

    private final int arity;

    private Matrix matrix;

    private boolean shared;

    protected int[] tuple;

    private long[][] nbInitConflicts;

    private long[] nbMaxConflicts;

    public AbstractMatrixManager(final Variable[] scope, final Matrix matrix,
            final boolean shared, final int[] tuple) {
        super();

        variables = scope;
        domainSize = new int[scope.length];

        for (int i = scope.length; --i >= 0;) {
            domainSize[i] = scope[i].getDomain().maxSize();
        }
        this.arity = scope.length;

        this.matrix = matrix;

        this.shared = shared;

        this.tuple = tuple;

    }

    private void countConflicts() {
        nbInitConflicts = nbConflicts();
        if (nbInitConflicts != null) {
            nbMaxConflicts = new long[arity];
            updateMaxConflicts();
        }
    }

    public final boolean set(final int[] tuple, final boolean status) {
        if (matrix.check(tuple) == status) {
            return false;
        }
        if (shared) {
            unshareMatrix();
        }
        matrix.set(tuple, status);
        if (!status) {
            addConflict(tuple);
        }
        return true;
    }

    @Override
    public final boolean removeTuple(final int[] tuple) {
        return set(tuple, false);
    }

    public final boolean isTrue(final int[] tuple) {
        return matrix.check(tuple);
    }

    public MatrixManager deepCopy(final Variable[] variables, final int[] tuple)
            throws CloneNotSupportedException {
        final AbstractMatrixManager clone = this.clone();
        clone.tuple = tuple;
        clone.variables = variables;
        return clone;
    }

    public AbstractMatrixManager clone() throws CloneNotSupportedException {
        return (AbstractMatrixManager) super.clone();
    }

    @Override
    public final boolean check() {
        // checks++;
        return matrix.check(tuple);
    }

    private void firstT() {
        for (int p = variables.length; --p >= 0;) {
            tuple[p] = variables[p].getDomain().maxSize() - 1;
        }
    }

    private boolean nextT() {
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

    public final void intersect(final Matrix matrix2) {
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
        try {
            matrix = matrix.clone();
            return matrix;
        } catch (CloneNotSupportedException e) {
            throw new IllegalStateException(e);
        }
    }

    @Override
    public final String getType() {
        return this.getClass().getSimpleName() + " w/ "
                + matrix.getClass().getSimpleName();
    }

    public final Matrix getMatrix() {
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

    @Override
    public final boolean supportCondition(final int position) {
        if (nbMaxConflicts == null) {
            countConflicts();
        }
        return getOtherSize(position) > nbMaxConflicts[position];
    }

    private long getOtherSize(final int position) {
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

    private long[][] nbConflicts() {
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
        //
        // LOGGER.fine("Counting " + this + " conflicts");

        if (matrix instanceof TupleSet) {

            final TupleSet tupleSet = (TupleSet) matrix;
            final boolean initialContent = tupleSet.getInitialContent();

            for (int[] tuple : tupleSet) {
                if (!initialContent) {
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

    public final void addConflict(final int[] tuple) {
        if (nbInitConflicts == null) {
            return;
        }
        for (int p = arity; --p >= 0;) {
            if (nbInitConflicts[p][tuple[p]]++ == nbMaxConflicts[p]) {
                nbMaxConflicts[p]++;
            }
        }
    }

    public final String toString() {
        return getClass().getSimpleName() + "\n" + matrix.toString();
    }
}
