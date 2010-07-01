package cspfj.generator.constraint;

import java.util.HashMap;
import java.util.Map;

import cspfj.exception.FailedGenerationException;
import cspfj.problem.Problem;
import cspom.constraint.CSPOMConstraint;

public final class GeneratorManager {

    private static final Map<String, Class<? extends Generator>> KNOWN = new HashMap<String, Class<? extends Generator>>();

    static {
        register("abs", AbsGenerator.class);
        register("add", AddGenerator.class);
        register("sub", AddGenerator.class);
        register("alldifferent", AllDifferentGenerator.class);
        register("or", DisjGenerator.class);
        register("not", DisjGenerator.class);
        register("eq", EqGenerator.class);
        register("neg", EqGenerator.class);
        register("ext", ExtensionGenerator.class);
        register("gt", GtGenerator.class);
        register("ge", GtGenerator.class);
        register("lt", GtGenerator.class);
        register("le", GtGenerator.class);
        register("mul", MulGenerator.class);
        register("ne", NeqGenerator.class);
        register("absdiff", AbsDiffGenerator.class);
        register("diffGe", DiffGeGenerator.class);
    }

    private final Problem problem;
    private final Map<Class<? extends Generator>, Generator> generators;

    public GeneratorManager(final Problem problem) {
        generators = new HashMap<Class<? extends Generator>, Generator>();
        this.problem = problem;
    }

    public static void register(final String description,
            final Class<? extends Generator> clazz) {

        KNOWN.put(description, clazz);

    }

    public boolean generate(final CSPOMConstraint constraint)
            throws FailedGenerationException {
        final Class<? extends Generator> candidate = KNOWN.get(constraint
                .getDescription());

        if (candidate == null) {
            throw new FailedGenerationException("No candidate constraint for "
                    + constraint + " (" + constraint.getDescription() + ")");
        }

        Generator generator = generators.get(candidate);
        if (generator == null) {
            try {
                generator = candidate.getConstructor(Problem.class)
                        .newInstance(problem);
            } catch (Exception e) {
                throw new FailedGenerationException(e);
            }
            generators.put(candidate, generator);
        }

        return generator.generate(constraint);
    }

}
