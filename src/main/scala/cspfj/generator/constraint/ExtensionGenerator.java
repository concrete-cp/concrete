package cspfj.generator.constraint;

import java.math.BigInteger;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.google.common.base.Function;
import com.google.common.base.Objects;
import com.google.common.collect.Iterables;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import com.google.common.primitives.Ints;

import cspfj.constraint.extension.ExtensionConstraints;
import cspfj.constraint.extension.Matrix;
import cspfj.constraint.extension.Matrix2D;
import cspfj.constraint.extension.MatrixGeneral;
import cspfj.constraint.extension.TupleSet;
import cspfj.exception.FailedGenerationException;
import cspfj.problem.Domain;
import cspfj.problem.Problem;
import cspfj.problem.Variable;
import cspom.constraint.CSPOMConstraint;
import cspom.extension.Relation;


public final class ExtensionGenerator extends AbstractGenerator {
    private static final int TIGHTNESS_LIMIT = 4;

    private final Map<Signature, Matrix> generated = new HashMap<Signature, Matrix>();

    public ExtensionGenerator(final Problem problem) {
        super(problem);
    }

    private Matrix generate(final List<Variable> variables,
            final Relation extension) {
        final List<Domain> domains = Lists.transform(variables,
                new Function<Variable, Domain>() {
                    @Override
                    public Domain apply(Variable input) {
                        return input.getDomain();
                    }
                });
        final Signature signature = new Signature(domains, extension);
        Matrix matrix = generated.get(signature);
        if (matrix == null) {
            matrix = bestMatrix(extension,
                    Lists.transform(domains, new Function<Domain, Integer>() {
                        @Override
                        public Integer apply(Domain input) {
                            return input.size();
                        }
                    }));
            fillMatrix(domains, extension, matrix);
            generated.put(signature, matrix);
        }
        return matrix;
    }

    @Override
    public boolean generate(final CSPOMConstraint constraint)
            throws FailedGenerationException {

        final List<Variable> solverVariables = Lists.transform(
                constraint.getScope(), cspomToCspfj);

        if (Iterables.any(solverVariables, NULL_DOMAIN)) {
            return false;
        }

        @SuppressWarnings("unchecked")
        final Matrix matrix = generate(solverVariables,
                ((cspom.extension.ExtensionConstraint<Number>) constraint)
                        .getRelation());

        addConstraint(ExtensionConstraints.newExtensionConstraint(matrix,
                solverVariables.toArray(new Variable[solverVariables.size()])));
        return true;

    }

    private static class Signature {
        private List<Domain> domains;
        private Extension<Number> extension;

        public Signature(final List<Domain> domains2,
                final Extension<Number> extension) {
            this.domains = domains2;
            this.extension = extension;
        }

        @Override
        public int hashCode() {
            return Objects.hashCode(domains.hashCode(), extension.hashCode());
        }

        @Override
        public boolean equals(final Object obj) {
            if (!(obj instanceof Signature)) {
                return false;
            }
            final Signature sign = (Signature) obj;
            return extension == sign.extension && domains.equals(sign.domains);
        }
    }

    private static boolean tupleSetBetterThanMatrix(final List<Integer> sizes,
            final int nbTuples) {
        BigInteger size = BigInteger.ONE;
        for (int i : sizes) {
            size = size.multiply(BigInteger.valueOf(i));
        }

        return size.compareTo(BigInteger.valueOf(Integer.MAX_VALUE)) >= 0
                || (TIGHTNESS_LIMIT * nbTuples < size.intValue());
    }

    private static Matrix bestMatrix(final Extension<Number> extension,
            final List<Integer> sizes) {

        if (extension.getArity() == 2) {
            return new Matrix2D(sizes.get(0), sizes.get(1), extension.init());
        }
        if (!extension.init()
                && tupleSetBetterThanMatrix(sizes, extension.getNbTuples())) {
            return new TupleSet(extension.getNbTuples(), extension.init());
        }
        return new MatrixGeneral(Ints.toArray(sizes), extension.init());

    }

    private static void fillMatrix(final List<Domain> domains,
            final Relation extension, final Matrix matrix) {
        final int[] tuple = new int[domains.size()];
        final List<Map<Number, Integer>> indexes = Lists.transform(domains,
                new Function<Domain, Map<Number, Integer>>() {
                    @Override
                    public Map<Number, Integer> apply(final Domain domain) {
                        return Maps.uniqueIndex(domain,
                                new Function<Integer, Number>() {
                                    @Override
                                    public Number apply(Integer input) {
                                        return domain.value(input);
                                    }
                                });
                    }
                });

        for (Number[] values : extension.getTuples()) {
            for (int i = tuple.length; --i >= 0;) {
                tuple[i] = indexes.get(i).get(values[i]);
            }
            matrix.set(tuple, !extension.init());
        }

    }
}
