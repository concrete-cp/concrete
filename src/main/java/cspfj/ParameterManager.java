package cspfj;

import java.lang.reflect.Field;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Properties;

import com.google.common.base.Joiner;

/**
 * This class is intended to hold CSP4J's various parameters.
 * 
 * @author vion
 * 
 */
public final class ParameterManager {

    private static final Map<String, Field> PARAMETERS = new HashMap<String, Field>();
    private static final Map<String, Object> PENDING = new HashMap<String, Object>();
    private static final Map<String, String> PENDING_PARSE = new HashMap<String, String>();

    public static void register(final Class<?> clazz) {
        for (Field f : clazz.getDeclaredFields()) {
            final cspfj.util.Parameter param = f
                    .getAnnotation(cspfj.util.Parameter.class);
            if (param != null) {
                final String name = param.value();
                PARAMETERS.put(param.value(), f);
                f.setAccessible(true);
                final Object value = PENDING.get(name);
                if (value != null) {
                    try {
                        f.set(null, value);
                    } catch (IllegalAccessException e) {
                        throw new IllegalStateException(e);
                    }
                    PENDING.remove(name);
                }
                final String s = PENDING_PARSE.get(name);
                if (s != null) {
                    try {
                        f.set(null, parse(f, s));
                    } catch (IllegalAccessException e) {
                        throw new IllegalStateException(e);
                    }
                    PENDING_PARSE.remove(name);
                }
            }
        }
    }

    /**
     * Updates some parameter, overriding default or previous value.
     * 
     * @param name
     * @param value
     */
    public static void parameter(final String name, final Object value) {
        final Field oldParameter = PARAMETERS.get(name);
        if (oldParameter == null) {
            PENDING.put(name, value);
        } else {
            try {
                oldParameter.set(null, value);
            } catch (IllegalAccessException e) {
                throw new IllegalStateException(e);
            }
        }
    }

    @SuppressWarnings("unchecked")
    private static Object parse(final Field field, final String value) {
        final Class<?> type = field.getType();
        if (Enum.class.isAssignableFrom(type)) {
            try {
                return Enum.valueOf(((Enum<?>) field.get(null)).getClass(),
                        value);
            } catch (IllegalAccessException e) {
                throw new IllegalStateException(e);
            }
        }
        if (Integer.class.equals(type) || int.class.equals(type)) {
            return Integer.parseInt(value);
        }
        if (Double.class.equals(type) || double.class.equals(type)) {
            return Double.parseDouble(value);
        }
        if (String.class.equals(type)) {
            return value;
        }
        if (Class.class.equals(type)) {
            try {
                return Class.forName(value);
            } catch (ClassNotFoundException e) {
                throw new IllegalArgumentException(e);
            }
        }
        throw new IllegalArgumentException("Cannot parse " + field
                + " of type " + type);
    }

    /**
     * Parses some parameter as a String, and converts it automatically using
     * given registered type.
     * 
     * @param name
     * @param value
     */
    public static void parameterParse(final String name, final String value) {
        final Field field = PARAMETERS.get(name);
        if (field == null) {
            PENDING_PARSE.put(name, value);
        } else {
            try {
                field.set(null, parse(field, value));
            } catch (IllegalAccessException e) {
                throw new IllegalStateException(e);
            }
        }
    }

    /**
     * Returns XML representation of the registered parameters.
     * 
     * @return
     */
    public static String toXML() {
        final StringBuilder stb = new StringBuilder();

        for (Entry<String, Field> p : PARAMETERS.entrySet()) {
            try {
                stb.append("\t\t\t<p name=\"").append(p.getKey()).append("\">")
                        .append(p.getValue().get(null)).append("</p>\n");
            } catch (IllegalAccessException e) {
                throw new IllegalStateException(e);
            }
        }

        return stb.toString();
    }

    /**
     * Returns String representation of the registered parameters.
     * 
     * @return
     */
    public static String list() {
        return "{"
                + Joiner.on(", ").withKeyValueSeparator("=").join(PARAMETERS)
                + "}";
    }

    public static void parseProperties(final Properties line) {
        for (Entry<Object, Object> e : line.entrySet()) {
            parameterParse(e.getKey().toString(), e.getValue().toString());
        }
    }

    private ParameterManager() {

    }

}
