package cspfj;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Properties;

/**
 * This class is intended to hold CSP4J's various parameters.
 * 
 * @author vion
 * 
 */
public final class ParameterManager {

    private static enum ParameterType {
        INTEGER, DOUBLE, STRING, OBJECT, CLASS, ENUM;

        private Object parse(String string) {
            switch (this) {
            case INTEGER:
                return Integer.valueOf(string);
            case DOUBLE:
                return Double.valueOf(string);
            case STRING:
                return string;
            case CLASS:
                try {
                    return Class.forName(string);
                } catch (ClassNotFoundException e) {
                    throw new IllegalArgumentException(e);
                }
            case ENUM:
            case OBJECT:
                throw new IllegalArgumentException("Cannot parse Object type");
            default:
                throw new IllegalStateException();
            }
        }
    }

    private static final class Parameter {
        private final ParameterType type;
        private final Object value;

        public Parameter(ParameterType type, Object value) {
            this.type = type;
            this.value = value;
        }
    }

    private static final Map<String, Parameter> PARAMETERS = new HashMap<String, Parameter>();
    private static final Map<String, Object> PENDING = new HashMap<String, Object>();
    private static final Map<String, String> PENDING_PARSE = new HashMap<String, String>();

    /**
     * Register Object type parameter
     * 
     * @param name
     * @param defaultValue
     */
    public static void registerObject(String name, Object defaultValue) {
        register(name, ParameterType.OBJECT, defaultValue);
    }

    /**
     * Register String type parameter
     * 
     * @param name
     * @param defaultValue
     */
    public static void registerString(String name, String defaultValue) {
        register(name, ParameterType.STRING, defaultValue);
    }

    /**
     * Register Integer type parameter
     * 
     * @param name
     * @param defaultValue
     */
    public static void registerInteger(String name, int defaultValue) {
        register(name, ParameterType.INTEGER, defaultValue);
    }

    /**
     * Register Double type parameter
     * 
     * @param name
     * @param defaultValue
     */
    public static void registerDouble(String name, double defaultValue) {
        register(name, ParameterType.DOUBLE, defaultValue);
    }

    /**
     * Register Class type parameter
     * 
     * @param name
     * @param defaultValue
     */
    public static void registerClass(String name, Class<?> defaultValue) {
        register(name, ParameterType.CLASS, defaultValue);
    }

    /**
     * Register Enum type parameter
     * 
     * @param <T>
     * @param name
     * @param defaultValue
     */
    public static <T extends Enum<T>> void registerEnum(String name,
            T defaultValue) {
        register(name, ParameterType.ENUM, defaultValue);
    }

    private static void register(String name, ParameterType type,
            Object defaultValue) {
        final Object value;
        final Object pending = PENDING.get(name);
        if (pending == null) {
            final String pendingParse = PENDING_PARSE.get(name);
            if (pendingParse == null) {
                value = defaultValue;
            } else {
                value = parse(type, defaultValue, pendingParse);
                PENDING_PARSE.remove(name);
            }
        } else {
            value = pending;
            PENDING.remove(name);
        }
        PARAMETERS.put(name, new Parameter(type, value));
    }

    /**
     * Updates some parameter, overriding default or previous value.
     * 
     * @param name
     * @param value
     */
    public static void parameter(String name, Object value) {
        final Parameter oldParameter = PARAMETERS.get(name);
        if (oldParameter == null) {
            PENDING.put(name, value);
        } else {
            PARAMETERS.put(name, new Parameter(oldParameter.type, value));
        }
    }

    private static Object parse(ParameterType type, Object oldValue,
            String value) {
        if (type == ParameterType.ENUM) {
            return Enum.valueOf(((Enum<?>) oldValue).getClass(), value);
        }
        return type.parse(value);
    }

    /**
     * Parses some parameter as a String, and converts it automatically using
     * given registered type.
     * 
     * @param name
     * @param value
     */
    public static void parameterParse(String name, String value) {
        final Parameter oldParameter = PARAMETERS.get(name);
        if (oldParameter == null) {
            PENDING_PARSE.put(name, value);
        } else {
            PARAMETERS.put(
                    name,
                    new Parameter(oldParameter.type, parse(oldParameter.type,
                            oldParameter.value, value)));
        }
    }

    /**
     * Returns current value for given parameter. Throws
     * IllegalArgumentException if the parameter has not been registered yet.
     * 
     * @param name
     * @return
     */
    public static Object getParameter(String name) {
        final Parameter parameter = PARAMETERS.get(name);
        if (parameter == null) {
            throw new IllegalArgumentException("Parameter is not registered");
        }
        return PARAMETERS.get(name).value;
    }

    /**
     * Returns XML representation of the registered parameters.
     * 
     * @return
     */
    public static String toXML() {
        final StringBuilder stb = new StringBuilder();

        for (Entry<String, Parameter> p : PARAMETERS.entrySet()) {
            stb.append("\t\t\t<p name=\"").append(p.getKey()).append("\">")
                    .append(p.getValue().value).append("</p>\n");
        }

        return stb.toString();
    }

    /**
     * Returns String representation of the registered parameters.
     * 
     * @return
     */
    public static String list() {
        final Iterator<Entry<String, Parameter>> i = PARAMETERS.entrySet()
                .iterator();
        if (!i.hasNext()) {
            return "{}";
        }

        final StringBuilder sb = new StringBuilder();
        sb.append('{');
        for (;;) {
            final Entry<String, Parameter> e = i.next();
            sb.append(e.getKey()).append('=').append(e.getValue().value);
            if (!i.hasNext()) {
                return sb.append('}').toString();
            }
            sb.append(", ");
        }
    }

    public static void parseProperties(Properties line) {
        for (Entry<Object, Object> e : line.entrySet()) {
            parameterParse(e.getKey().toString(), e.getValue().toString());
        }
    }

    private ParameterManager() {

    }

}
