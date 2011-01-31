package cspfj;

import static com.google.common.base.Preconditions.checkNotNull;
import static com.google.common.collect.Maps.newHashMap;

import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.util.Arrays;
import java.util.Map;
import java.util.Map.Entry;
import java.util.SortedMap;
import java.util.logging.Logger;

import com.google.common.base.Function;
import com.google.common.base.Predicate;
import com.google.common.base.Predicates;
import com.google.common.collect.BiMap;
import com.google.common.collect.HashBiMap;
import com.google.common.collect.HashMultimap;
import com.google.common.collect.Iterables;
import com.google.common.collect.Maps;
import com.google.common.collect.Multimap;

public final class StatisticsManager {

    private static final Map<String, Field> STATIC_STATISTICS = newHashMap();

    private static final BiMap<Object, String> REFD_OBJECTS = HashBiMap
            .create();

    private static final Multimap<Object, Field> DYNAMIC_FIELDS = HashMultimap
            .create();

    private static final Predicate<Field> ANNOTED_FIELD = new Predicate<Field>() {
        @Override
        public boolean apply(final Field input) {
            return input.getAnnotation(cspfj.util.Statistic.class) != null;
        }
    };
    private static final Predicate<Field> STATIC_FIELD = new Predicate<Field>() {
        @Override
        public boolean apply(final Field input) {
            return (input.getModifiers() & Modifier.STATIC) != 0;
        }
    };
    private static final Function<Field, Object> STATIC_FIELD_TO_VALUE = new Function<Field, Object>() {
        @Override
        public Object apply(final Field value) {
            try {
                return value.get(null);
            } catch (IllegalAccessException e) {
                throw new IllegalStateException(e);
            }
        }
    };

    private static final Logger LOGGER = Logger
            .getLogger(StatisticsManager.class.getName());

    private StatisticsManager() {

    }

    public static void register(final Class<?> clazz) {
        for (Field f : Iterables.filter(
                Arrays.asList(clazz.getDeclaredFields()),
                Predicates.and(STATIC_FIELD, ANNOTED_FIELD))) {
            STATIC_STATISTICS.put(clazz.getName() + '.' + f.getName(), f);
            f.setAccessible(true);
        }
    }

    public static void register(final String name, final Object object) {
        final Object oldObject = REFD_OBJECTS.inverse().get(name);
        if (oldObject != null) {
            LOGGER.info(name
                    + ": an object with the same name is already registered");
            DYNAMIC_FIELDS.removeAll(oldObject);
            REFD_OBJECTS.remove(oldObject);
        }

        REFD_OBJECTS.put(object, name);
        final Class<?> clazz = object.getClass();
        for (Field f : Iterables.filter(
                Arrays.asList(clazz.getDeclaredFields()),
                Predicates.and(ANNOTED_FIELD, Predicates.not(STATIC_FIELD)))) {
            DYNAMIC_FIELDS.put(object, f);
        }
    }

    public static Object get(final String name) {
        final Field staticField = STATIC_STATISTICS.get(name);
        if (staticField == null) {
            final int fieldNameAt = name.lastIndexOf('.');
            final Object obj = checkNotNull(REFD_OBJECTS.inverse().get(
                    name.substring(0, fieldNameAt)));
            final String fieldName = name.substring(fieldNameAt + 1,
                    name.length());
            for (Field f : DYNAMIC_FIELDS.get(obj)) {
                if (f.getName().equals(fieldName)) {
                    try {
                        return f.get(obj);
                    } catch (IllegalAccessException e) {
                        throw new IllegalStateException(e);
                    }
                }
            }
            throw new IllegalArgumentException("Could not find " + name);
        }
        return STATIC_FIELD_TO_VALUE.apply(staticField);

    }

    public static Map<String, Object> digest() {
        final SortedMap<String, Object> digest = Maps.newTreeMap();
        digest.putAll(Maps.transformValues(STATIC_STATISTICS,
                STATIC_FIELD_TO_VALUE));

        for (Entry<Object, Field> e : DYNAMIC_FIELDS.entries()) {
            try {
                digest.put(REFD_OBJECTS.get(e.getKey()) + "."
                        + e.getValue().getName(), e.getValue().get(e.getKey()));
            } catch (IllegalAccessException e1) {
                throw new IllegalStateException(e1);
            }
        }
        return digest;
    }

    private static boolean isIntType(final Class<?> input) {
        return Integer.class.equals(input) || int.class.equals(input)
                || Long.class.equals(input) || long.class.equals(input);
    }

    private static boolean isFloatType(final Class<?> input) {
        return Float.class.equals(input) || float.class.equals(input)
                || Double.class.equals(input) || double.class.equals(input);
    }

    public static void reset() {
        REFD_OBJECTS.clear();
        DYNAMIC_FIELDS.clear();
        for (Field f : STATIC_STATISTICS.values()) {
            try {
                if (isIntType(f.getType())) {
                    f.set(null, 0);
                } else if (isFloatType(f.getType())) {
                    f.set(null, 0f);
                }
            } catch (IllegalAccessException e) {
                throw new IllegalStateException(e);
            }
        }
    }

}
