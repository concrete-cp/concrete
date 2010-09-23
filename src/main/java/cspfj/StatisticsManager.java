package cspfj;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

public class StatisticsManager {

    private static final Collection<Field> STATISTICS = new ArrayList<Field>();

    private StatisticsManager() {

    }

    public static void register(Field field) {
        STATISTICS.add(field);
    }

    public static Map<String, Object> digest() {
        final Map<String, Object> digest = new HashMap<String, Object>();
        for (Field f : STATISTICS) {
            try {
                digest.put(f.getDeclaringClass() + "." + f.getName(),
                        f.get(null));
            } catch (IllegalAccessException e) {
                throw new IllegalStateException(e);
            }
        }
        return digest;
    }

}
