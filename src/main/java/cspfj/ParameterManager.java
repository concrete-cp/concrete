package cspfj;

import java.lang.annotation.Annotation;
import java.lang.reflect.Field;
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

	private static final Map<String, Field> PARAMETERS = new HashMap<String, Field>();
	private static final Map<String, Object> PENDING = new HashMap<String, Object>();
	private static final Map<String, String> PENDING_PARSE = new HashMap<String, String>();

	public static void register(Class<?> clazz) {
		for (Field f : clazz.getDeclaredFields()) {
			final Annotation a = f.getAnnotation(cspfj.util.Parameter.class);
			if (a != null) {
				final String name = ((cspfj.util.Parameter) a).value();
				PARAMETERS.put(((cspfj.util.Parameter) a).value(), f);
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
	public static void parameter(String name, Object value) {
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

	private static Object parse(Field field, String value) {
		final Class<?> type = field.getType();
		if (Enum.class.isAssignableFrom(type)) {
			try {
				return Enum.valueOf(((Enum<?>) field.get(null)).getClass(),
						value);
			} catch (IllegalAccessException e) {
				throw new IllegalStateException(e);
			}
		}
		if (Integer.class.equals(type) || Integer.TYPE.equals(type)) {
			return Integer.parseInt(value);
		}
		if (Double.class.equals(type) || Double.TYPE.equals(type)) {
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
	public static void parameterParse(String name, String value) {
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
	 * Returns current value for given parameter. Throws
	 * IllegalArgumentException if the parameter has not been registered yet.
	 * 
	 * @param name
	 * @return
	 */
	@Deprecated
	public static Object getParameter(String name) {
		final Field field = PARAMETERS.get(name);
		if (field == null) {
			throw new IllegalArgumentException("Parameter is not registered");
		}
		try {
			return field.get(null);
		} catch (IllegalAccessException e) {
			throw new IllegalStateException(e);
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
		final Iterator<Entry<String, Field>> i = PARAMETERS.entrySet()
				.iterator();
		if (!i.hasNext()) {
			return "{}";
		}

		final StringBuilder sb = new StringBuilder();
		sb.append('{');
		for (;;) {
			final Entry<String, Field> e = i.next();
			try {
				sb.append(e.getKey()).append('=')
						.append(e.getValue().get(null));
			} catch (IllegalAccessException e1) {
				throw new IllegalStateException(e1);
			}
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
