package cspfj.problem;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Map.Entry;
import java.util.logging.Logger;

import cspfj.constraint.Constraint;
import cspfj.constraint.DynamicConstraint;
import cspfj.constraint.extension.ExtensionConstraint2D;
import cspfj.constraint.extension.ExtensionConstraintGeneral;
import cspfj.constraint.extension.Matrix;
import cspfj.constraint.extension.Matrix2D;
import cspfj.constraint.extension.TupleSet;
import cspfj.constraint.semantic.RCConstraint;
import cspfj.util.BitVector;

public final class NoGoodLearner {

    public static enum LearnMethod {
        NONE, EXT, RC, BIN
    }

    private static final Logger LOGGER = Logger.getLogger(NoGoodLearner.class
            .getName());

    private static boolean useNoGoods = true;

    private static int nbNoGoods = 0;

    private NoGoodLearner() {

    }

    public static int getNbNoGoods() {
        return nbNoGoods;
    }

    public static boolean noGoods(final Problem problem,
            final LearnMethod learnMethod) {
        if (!useNoGoods) {
            return false;
        }

        if (problem.getLevelVariable(0) == null) {
            return false;
        }

        // final Map<Variable[], List<int[]>> noGoods = new HashMap<Variable[],
        // List<int[]>>();

        int startLevel = 0;
        while (startLevel < problem.getNbVariables()
                && problem.getLevelVariable(startLevel) != null) {
            startLevel++;
        }

        int[] tuple = new int[startLevel + 1];

        final Set<Variable> scopeSet = new HashSet<Variable>(startLevel);
        for (int i = startLevel; --i >= 0;) {
            scopeSet.add(problem.getLevelVariable(i));
            tuple[i] = problem.getLevelVariable(i).getFirst();
        }

        boolean modified = false;
        final Collection<Constraint> addedConstraints = new ArrayList<Constraint>();

        for (int level = startLevel + 1; --level >= 1;) {
            // Note : Nothing to remove on first level
            scopeSet.remove(problem.getLevelVariable(level));
            final Variable[] scopeArray = problem
                    .getLevelVariablesTo(level + 1);
            // restoreLevel(level);

            tuple = Arrays.copyOf(tuple, level + 1);

            for (Variable fv : problem.getVariables()) {

                // logger.fine("checking " +
                // getVariable(levelVariables[level-1]));

                if (scopeSet.contains(fv)) {
                    continue;
                }

                final BitVector changes = fv.getDomain().getAtLevel(level - 1)
                        .xor(fv.getDomain().getAtLevel(level));
                if (changes.isEmpty()) {
                    continue;
                }

                scopeSet.add(fv);
                final DynamicConstraint constraint = learnConstraint(problem,
                        scopeSet, learnMethod);
                scopeSet.remove(fv);

                if (constraint == null) {
                    continue;
                }

                scopeArray[level] = fv;

                final int[] base = new int[constraint.getArity()];
                final int varPos = makeBase(scopeArray, tuple, constraint, base);

                int newNogoods = 0;
                for (int i = changes.nextSetBit(0); i >= 0; i = changes
                        .nextSetBit(i + 1)) {
                    base[varPos] = i;
                    newNogoods += constraint.removeTuples(base);

                }
                if (newNogoods > 0) {
                    nbNoGoods += newNogoods;
                    modified = true;
                    if (constraint.getId() > problem.getMaxCId()) {
                        LOGGER.info("Added " + constraint);
                        addedConstraints.add(constraint);
                    }
                }
            }
        }
        if (modified) {
            LOGGER.info(nbNoGoods + " nogoods");

            if (!addedConstraints.isEmpty()) {
                for (Constraint c : addedConstraints) {
                    problem.addConstraint(c);
                }
                problem.prepareConstraints();
                LOGGER.info(problem.getNbConstraints() + " constraints");
            }
        }
        return modified;
    }

    private static int[] makeBase(final Variable[] scope, final int[] values,
            final Constraint constraint) {
        assert scope.length == values.length;
        int[] tuple = new int[constraint.getArity()];

        Arrays.fill(tuple, -1);

        for (int i = constraint.getArity(); --i >= 0;) {
            final Variable var = constraint.getVariable(i);
            for (int j = scope.length; --j >= 0;) {
                if (scope[j] == var) {
                    tuple[i] = values[j];
                    break;
                }
            }
        }

        return tuple;
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
    public static int makeBase(final Variable[] scope, final int[] values,
            final Constraint constraint, final int[] base) {
        assert scope.length == values.length;
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
                    base[i] = values[j];
                    break;
                }
            }

        }

        return positionInConstraint;
    }

    public static boolean noGoodsToConstraints(
            final Map<Variable[], List<int[]>> noGoods, final Problem problem,
            final LearnMethod learnMethod) {
        if (noGoods == null) {
            return false;
        }
        boolean modified = false;
        final Collection<Constraint> addedConstraints = new ArrayList<Constraint>();

        for (Entry<Variable[], List<int[]>> e : noGoods.entrySet()) {
            final Set<Variable> scope = new HashSet<Variable>(Arrays.asList(e
                    .getKey()));

            final DynamicConstraint constraint = learnConstraint(problem,
                    scope, learnMethod);

            if (constraint == null) {
                continue;
            }

            int newNogoods = 0;
            for (int[] tuple : e.getValue()) {
                newNogoods += constraint.removeTuples(makeBase(e.getKey(),
                        tuple, constraint));
            }
            if (newNogoods == 0) {
                continue;
            }
            nbNoGoods += newNogoods;
            modified = true;
            if (constraint.getId() > problem.getMaxCId()) {
                LOGGER.info("Added " + constraint);
                addedConstraints.add(constraint);
            }

        }

        if (modified) {
            LOGGER.info(nbNoGoods + " nogoods");

            if (!addedConstraints.isEmpty()) {
                for (Constraint c : addedConstraints) {
                    problem.addConstraint(c);
                }
                problem.prepareConstraints();
                LOGGER.info(problem.getNbConstraints() + " constraints");
            }
        }
        return modified;
    }

    private static DynamicConstraint findDynamicConstraint(
            final Set<Variable> scope) {
        for (DynamicConstraint c : scope.iterator().next()
                .getDynamicConstraints()) {
            if (c.getArity() == scope.size()
            // || (c.getArity() > scope.size() && c
            // .positive()))
                    && c.getScopeSet().containsAll(scope)) {
                return c;
            }
        }
        return null;
    }

    public static DynamicConstraint learnConstraint(final Problem problem,
            final Set<Variable> scope, final LearnMethod learnMethod) {
        final DynamicConstraint constraint = findDynamicConstraint(scope);

        if (constraint != null) {
            return constraint;
        }

        if (learnMethod == LearnMethod.NONE) {
            return null;
        }

        final int level = scope.size();
        final Variable[] constraintScope = scope.toArray(new Variable[level]);

        if (level == 2) {

            if (learnMethod == LearnMethod.RC) {
                return new RCConstraint(constraintScope);
            }

            final Matrix2D matrix = new Matrix2D(constraintScope[0].getDomain()
                    .maxSize(), constraintScope[1].getDomain().maxSize(), true);
            return new ExtensionConstraint2D(constraintScope, matrix, false);

        }

        if (learnMethod == LearnMethod.EXT) {

            final Matrix matrix = new TupleSet(true);

            return new ExtensionConstraintGeneral(matrix, false,
                    constraintScope);

        }
        return null;
    }

}
