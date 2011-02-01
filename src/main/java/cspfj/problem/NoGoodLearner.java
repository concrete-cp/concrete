package cspfj.problem;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Deque;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.Set;

import com.google.common.base.Predicate;
import com.google.common.collect.ImmutableSet;
import com.google.common.collect.Iterables;

import cspfj.constraint.Constraint;
import cspfj.constraint.DynamicConstraint;
import cspfj.constraint.extension.ExtensionConstraint2D;
import cspfj.constraint.extension.ExtensionConstraintGeneral;
import cspfj.constraint.extension.Matrix;
import cspfj.constraint.extension.Matrix2D;
import cspfj.constraint.extension.TupleSet;
import cspfj.heuristic.Pair;
import cspfj.util.BitVector;
import cspfj.util.Statistic;

public final class NoGoodLearner {

    public static enum LearnMethod {
        NONE, CONSERVATIVE, EXT, BIN;
    }

    // private static final Logger LOGGER =
    // Logger.getLoggerS(NoGoodLearner.class
    // .getName());

    @Statistic
    public int nbNoGoods = 0;

    private final Problem problem;
    private final LearnMethod learnMethod;

    public NoGoodLearner(final Problem problem, final LearnMethod learnMethod) {
        this.problem = problem;
        this.learnMethod = learnMethod;
    }

    public Set<Constraint> noGoods(final Deque<Pair> decisions) {
        if (LearnMethod.NONE.equals(learnMethod) || decisions.isEmpty()) {
            return ImmutableSet.of();
        }

        final Set<Constraint> modifiedConstraints = new HashSet<Constraint>();

        // final Map<Variable[], List<int[]>> noGoods = new HashMap<Variable[],
        // List<int[]>>();

        // final Deque<Pair> workingDecisions = new LinkedList<Pair>(decisions);

        final Collection<Constraint> addedConstraints = new ArrayList<Constraint>();

        final List<Integer> tuple = new ArrayList<Integer>(decisions.size() + 1);
        final LinkedHashSet<Variable> scope = new LinkedHashSet<Variable>(
                decisions.size() + 1);
        int level = 0;
        while (!decisions.isEmpty()) {
            if (++level >= problem.getMaxArity()
                    && learnMethod != LearnMethod.EXT) {
                break;
            }
            final Pair lastDecision = decisions.removeLast();
            tuple.add(lastDecision.getIndex());
            scope.add(lastDecision.getVariable());

            for (Variable fv : problem.getVariables()) {

                // logger.fine("checking " +
                // getVariable(levelVariables[level-1]));

                if (scope.contains(fv)) {
                    continue;
                }

                final BitVector changes = fv.getDomain().getAtLevel(level - 1)
                        .xor(fv.getDomain().getAtLevel(level));
                if (changes.isEmpty()) {
                    continue;
                }

                scope.add(fv);
                final DynamicConstraint constraint = learnConstraint(scope);

                if (constraint != null) {
                    final int[] base = new int[constraint.getArity()];
                    final int varPos = makeBase(
                            scope.toArray(new Variable[scope.size()]), tuple,
                            constraint, base);

                    int newNogoods = 0;
                    for (int i = changes.nextSetBit(0); i >= 0; i = changes
                            .nextSetBit(i + 1)) {
                        base[varPos] = i;
                        newNogoods += constraint.removeTuples(base);

                    }
                    if (newNogoods > 0) {
                        nbNoGoods += newNogoods;
                        modifiedConstraints.add(constraint);
                        if (constraint.getId() > problem.getMaxCId()) {
                            // LOGGER.info("Added " + constraint);
                            addedConstraints.add(constraint);
                        }
                    }
                }
                scope.remove(fv);
            }
        }
        if (!modifiedConstraints.isEmpty()) {
            // LOGGER.info(nbNoGoods + " nogoods");

            if (!addedConstraints.isEmpty()) {
                for (Constraint c : addedConstraints) {
                    problem.addConstraint(c);
                }
                problem.prepare();
                // LOGGER.info(problem.getNbConstraints() + " constraints");
            }
        }
        return modifiedConstraints;
    }

    /**
     * Sets the base array given as a parameter so that the values of base
     * correspond to the values of the values array reordered such that they
     * correspond to the variables of the scope of the constraint. Variables
     * present in the scope of the constraint but not in the scope[] array
     * result in a -1 value in the base[] array. Last variable of scope[] is
     * ignored. Returns the position of the last variable of scope[] in the
     * constraint's scope.
     * 
     * @param scope
     * @param values
     * @param constraint
     * @param base
     * @return
     */
    public static int makeBase(final Variable[] scope,
            final List<Integer> values, final Constraint constraint,
            final int[] base) {
        assert scope.length == values.size() + 1;
        assert base.length == constraint.getArity();

        Arrays.fill(base, -1);

        final Variable seek = scope[scope.length - 1];
        int positionInConstraint = -1;

        for (int i = constraint.getArity(); --i >= 0;) {
            final Variable var = constraint.getVariable(i);
            if (var == seek) {
                positionInConstraint = i;
                continue;
            }
            for (int j = scope.length - 1; --j >= 0;) {
                if (scope[j] == var) {
                    base[i] = values.get(j);
                    break;
                }
            }

        }

        return positionInConstraint;
    }

    public DynamicConstraint learnConstraint(final Set<Variable> scope) {
        try {
            return Iterables.find(Iterables.getFirst(scope, null)
                    .getDynamicConstraints(),
                    new Predicate<DynamicConstraint>() {
                        @Override
                        public boolean apply(DynamicConstraint constraint) {
                            return constraint.getArity() == scope.size()
                                    && constraint.getScopeSet().containsAll(
                                            scope);
                        }
                    });
        } catch (NoSuchElementException e) {
            final int level = scope.size();
            switch (learnMethod) {
            case BIN:
                if (level != 2) {
                    return null;
                }
            case EXT:
                if (level == 2) {

                    final Variable[] constraintScope = scope
                            .toArray(new Variable[level]);

                    final Matrix2D matrix = new Matrix2D(constraintScope[0]
                            .getDomain().maxSize(), constraintScope[1]
                            .getDomain().maxSize(), true);
                    return new ExtensionConstraint2D(constraintScope, matrix,
                            false);

                }

                final Variable[] constraintScope = scope
                        .toArray(new Variable[level]);

                final Matrix matrix = new TupleSet(true);

                return new ExtensionConstraintGeneral(matrix, false,
                        constraintScope);
            default:
                return null;
            }

        }
    }

    public final LearnMethod getLearnMethod() {
        return learnMethod;
    }

}
