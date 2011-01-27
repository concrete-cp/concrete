package cspfj;

import static com.google.common.collect.Maps.newHashMap;

import java.lang.annotation.Annotation;
import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.util.Arrays;
import java.util.Map;
import java.util.Map.Entry;
import java.util.SortedMap;

import com.google.common.base.Function;
import com.google.common.base.Predicate;
import com.google.common.collect.HashMultimap;
import com.google.common.collect.Iterables;
import com.google.common.collect.Maps;
import com.google.common.collect.Maps.EntryTransformer;
import com.google.common.collect.Multimap;

public class StatisticsManager {

    private static final Map<String, Field> STATIC_STATISTICS = newHashMap();

    private static Map<Object, String> REFERENCED_OBJECTS = newHashMap();

    private static Multimap<Object, Field> DYNAMIC_FIELDS = HashMultimap
            .create();

    private static final Predicate<Field> ANNOTED_FIELD = new Predicate<Field>() {
        @Override
        public boolean apply(Field input) {
            return input.getAnnotation(cspfj.util.Statistic.class) != null;
        }
    };

    private StatisticsManager() {

    }

    public static void register(Class<?> clazz) {
        for (Field f : Iterables.filter(
                Arrays.asList(clazz.getDeclaredFields()), ANNOTED_FIELD)) {
            if ((f.getModifiers() & Modifier.STATIC) != 0) {
                STATIC_STATISTICS.put(f.getDeclaringClass().toString() + '.'
                        + f.getName(), f);
                f.setAccessible(true);
            }
        }
    }

    public static void register(String name, Object object) {
        REFERENCED_OBJECTS.put(object, name);
        final Class<?> clazz = object.getClass();
        for (Field f : Iterables.filter(
                Arrays.asList(clazz.getDeclaredFields()), ANNOTED_FIELD)) {
            DYNAMIC_FIELDS.put(object, f);
        }
    }

    public static Map<String, Object> digest() {
        final SortedMap<String, Object> digest = Maps.newTreeMap();
        digest.putAll(Maps.transformValues(STATIC_STATISTICS,
                new Function<Field, Object>() {
                    @Override
                    public Object apply(Field value) {
                        try {
                            return value.get(null);
                        } catch (IllegalAccessException e) {
                            throw new IllegalStateException(e);
                        }
                    }
                }));

        for (Entry<Object, Field> e : DYNAMIC_FIELDS.entries()) {
            try {
                digest.put(REFERENCED_OBJECTS.get(e.getKey()) + "."
                        + e.getValue().getName(), e.getValue().get(e.getKey()));
            } catch (IllegalAccessException e1) {
                throw new IllegalStateException(e1);
            }
        }
        return digest;
    }

    public static void reset() {
        REFERENCED_OBJECTS.clear();
        DYNAMIC_FIELDS.clear();
        for (Field f : STATIC_STATISTICS.values()) {
            if (Integer.class.equals(f.getType())
                    || int.class.equals(f.getType())
                    || Long.class.equals(f.getType())
                    || Long.TYPE.equals(f.getType())) {
                try {
                    f.set(null, 0);
                } catch (IllegalAccessException e) {
                    throw new IllegalStateException(e);
                }
            } else if (Double.class.equals(f.getType())
                    || Double.TYPE.equals(f.getType())) {
                try {
                    f.set(null, 0.0);
                } catch (IllegalAccessException e) {
                    throw new IllegalStateException(e);
                }
            }
        }
    }

}
