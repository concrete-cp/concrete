package cspfj.problem;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Deque;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.ListIterator;
import java.util.NoSuchElementException;
import java.util.Set;

import com.google.common.base.Predicate;
import com.google.common.base.Predicates;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.Iterables;
import com.google.common.collect.Lists;
import com.google.common.collect.Sets;

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
            return Collections.emptySet();
        }

        final Set<Constraint> modifiedConstraints = new HashSet<Constraint>();

        final Collection<Constraint> addedConstraints = new ArrayList<Constraint>();

        final List<Integer> tuple = new ArrayList<Integer>(decisions.size() + 1);
        final Set<Variable> futureVariables = Sets.newHashSet(problem
                .getVariables());
        final LinkedList<Variable> currentScope = Lists.newLinkedList();
        int level = 0;
        while (!decisions.isEmpty()) {
            if (++level >= problem.getMaxArity()
                    && learnMethod != LearnMethod.EXT) {
                break;
            }
            /*
             * Decisions are stacked, so the first decision in the search tree
             * is actually the last in the stack.
             */
            final Pair lastDecision = decisions.pollLast();
            tuple.add(lastDecision.getIndex());
            currentScope.add(lastDecision.getVariable());
            futureVariables.remove(lastDecision.getVariable());

            for (Variable fv : futureVariables) {

                // logger.fine("checking " +
                // getVariable(levelVariables[level-1]));

                final BitVector changes = fv.getDomain().getAtLevel(level - 1)
                        .xor(fv.getDomain().getAtLevel(level));
                if (changes.isEmpty()) {
                    continue;
                }

                currentScope.add(fv);
                final DynamicConstraint constraint = learnConstraint(currentScope);

                if (constraint != null) {
                    final int[] base = new int[constraint.getArity()];
                    final int varPos = makeBase(currentScope, tuple,
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
                currentScope.removeLast();
            }
        }

        if (!addedConstraints.isEmpty()) {
            for (Constraint c : addedConstraints) {
                problem.addConstraint(c);
            }
            problem.prepare();
            // LOGGER.info(problem.getNbConstraints() + " constraints");
        }

        return modifiedConstraints;
    }

    public Set<Constraint> binNoGoods(final Variable firstVariable) {
        final List<Integer> tuple = ImmutableList.of(firstVariable.getFirst());
        final Set<Constraint> modifiedConstraints = new HashSet<Constraint>();
        final LinkedList<Variable> scope = Lists.newLinkedList();
        scope.add(firstVariable);

        final Collection<Constraint> addedConstraints = new ArrayList<Constraint>();

        for (Variable fv : Iterables.filter(
                Arrays.asList(problem.getVariables()),
                Predicates.not(Predicates.equalTo(firstVariable)))) {

            // logger.fine("checking " +
            // getVariable(levelVariables[level-1]));

            final BitVector changes = fv.getDomain().getAtLevel(0)
                    .xor(fv.getDomain().getAtLevel(1));
            if (changes.isEmpty()) {
                continue;
            }
            scope.add(fv);
            final DynamicConstraint constraint = learnConstraint(scope);

            if (constraint == null) {
                continue;
            }

            final int[] base = new int[constraint.getArity()];
            final int varPos = NoGoodLearner.makeBase(scope, tuple, constraint,
                    base);

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
                    addedConstraints.add(constraint);
                }
            }
            scope.removeLast();
        }

        if (!addedConstraints.isEmpty()) {
            for (Constraint c : addedConstraints) {
                problem.addConstraint(c);
            }

            problem.prepare();
        }

        return modifiedConstraints;
    }

    /**
     * Sets the base array given as a parameter so that the values of base
     * correspond to the values of the values array reordered such that they
     * correspond to the variables of the scope of the constraint. Variables
     * present in the scope of the constraint but not in the scope[] array
     * result in a 0 value in the base[] array. Last variable of scope[] is
     * ignored. Returns the position of the last variable of scope[] in the
     * constraint's scope.
     * 
     * @param scope
     * @param values
     * @param constraint
     * @param base
     * @return
     */
    public static int makeBase(final LinkedList<Variable> scope,
            final List<Integer> values, final Constraint constraint,
            final int[] base) {
        assert scope.size() == values.size() + 1;
        assert base.length == constraint.getArity();

        for (ListIterator<Variable> itr = scope.subList(0, scope.size() - 1)
                .listIterator(); itr.hasNext();) {
            final int index = itr.nextIndex();
            base[constraint.getPosition(itr.next())] = values.get(index);
        }

        return constraint.getPosition(scope.getLast());
    }

    public DynamicConstraint learnConstraint(final Collection<Variable> scope) {
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

                final Variable[] constraintScope = scope
                        .toArray(new Variable[level]);

                if (level == 2) {

                    final Matrix2D matrix = new Matrix2D(constraintScope[0]
                            .getDomain().maxSize(), constraintScope[1]
                            .getDomain().maxSize(), true);
                    return new ExtensionConstraint2D(constraintScope, matrix,
                            false);

                }

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
