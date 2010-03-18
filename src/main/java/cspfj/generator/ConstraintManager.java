package cspfj.generator;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;

import cspfj.constraint.Constraint;
import cspfj.exception.FailedGenerationException;
import cspfj.problem.Problem;
import cspfj.problem.Variable;
import cspom.constraint.CSPOMConstraint;
import cspom.variable.CSPOMVariable;

public class ConstraintManager {

    private final static Map<String, Collection<Class<? extends Constraint>>> KNOWN = new HashMap<String, Collection<Class<? extends Constraint>>>();

    public static void register(final String description,
            final Class<? extends Constraint> clazz) {
        Collection<Class<? extends Constraint>> known = KNOWN.get(description);
        if (known == null) {
            known = new ArrayList<Class<? extends Constraint>>();
            KNOWN.put(description, known);
        }
        known.add(clazz);
    }

    public static boolean generate(CSPOMConstraint constraint, Problem problem)
            throws FailedGenerationException {
        final Collection<Class<? extends Constraint>> candidates = KNOWN
                .get(constraint.getDescription());

        if (candidates == null) {
            throw new FailedGenerationException("No candidate constraint for "
                    + constraint + " (" + constraint.getDescription() + ")");
        }

        for (Class<? extends Constraint> candidate : candidates) {

            final Method generate;
            try {
                generate = candidate.getMethod("generate",
                        CSPOMConstraint.class, Problem.class);
            } catch (SecurityException e) {
                throw new FailedGenerationException(e);
            } catch (NoSuchMethodException e) {
                throw new FailedGenerationException(e);
            }

            try {
                if ((Boolean) generate.invoke(null, constraint, problem)) {
                    return true;
                }
            } catch (IllegalArgumentException e) {
                throw new FailedGenerationException(e);
            } catch (IllegalAccessException e) {
                throw new FailedGenerationException(e);
            } catch (InvocationTargetException e) {
                throw new FailedGenerationException(e);
            }

        }
        return false;
    }

    public static Variable[] getSolverVariables(List<CSPOMVariable> variables,
            Problem problem) {
        final Variable[] solverVariables = new Variable[variables.size()];
        for (ListIterator<CSPOMVariable> itr = variables.listIterator(); itr
                .hasNext();) {
            solverVariables[itr.nextIndex()] = problem.getSolverVariable(itr
                    .next());
        }
        return solverVariables;
    }
}
