package cspfj.generator;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import cspfj.constraint.Constraint;
import cspfj.problem.Variable;

public class ConstraintManager {

    private static final Map<String, Collection<Class<? extends Constraint>>> CONSTRAINTS = new HashMap<String, Collection<Class<? extends Constraint>>>();

    public static void register(final String reference,
            final Class<? extends Constraint> constraintClass) {
        Collection<Class<? extends Constraint>> list = CONSTRAINTS
                .get(reference);
        if (list == null) {
            list = new ArrayList<Class<? extends Constraint>>();
            CONSTRAINTS.put(reference, list);
        }
        list.add(constraintClass);
    }

    public static Constraint generate(final String reference,
            final Variable[] signature) throws SecurityException,
            NoSuchMethodException, IllegalArgumentException,
            IllegalAccessException, InvocationTargetException,
            InstantiationException {
        final Collection<Class<? extends Constraint>> list = CONSTRAINTS
                .get(reference);
        if (list == null) {
            return null;
        }
        for (Class<? extends Constraint> constraintClass : list) {
            final Method m = constraintClass.getMethod("isApplicable",
                    Variable[].class);
            if (Boolean.TRUE.equals(m.invoke(null, (Object[]) signature))) {
                return constraintClass.getConstructor(Variable[].class)
                        .newInstance((Object[]) signature);
            }
        }
        return null;
    }
}
