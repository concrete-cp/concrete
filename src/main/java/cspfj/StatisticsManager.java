package cspfj;

import java.lang.annotation.Annotation;
import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.SortedMap;
import java.util.TreeMap;

public class StatisticsManager {

	private static final Collection<Field> STATIC_STATISTICS = new ArrayList<Field>();

	private static Map<String, Object> REFERENCED_OBJECTS = new HashMap<String, Object>();

	private StatisticsManager() {

	}

	public static void register(Class<?> clazz) {
		for (Field f : clazz.getDeclaredFields()) {
			if ((f.getModifiers() & Modifier.STATIC) != 0) {
				final Annotation a = f
						.getAnnotation(cspfj.util.Statistic.class);
				if (a != null) {
					STATIC_STATISTICS.add(f);
					f.setAccessible(true);
				}
			}
		}
	}

	public static void register(String name, Object object) {
		if (REFERENCED_OBJECTS.containsKey(name)) {
			throw new IllegalArgumentException(name + " is already registered");
		}
		REFERENCED_OBJECTS.put(name, object);
	}

	public static Map<String, Object> digest() {
		final SortedMap<String, Object> digest = new TreeMap<String, Object>();
		for (Field f : STATIC_STATISTICS) {
			try {
				digest.put(f.getDeclaringClass() + "." + f.getName(),
						f.get(null));
			} catch (IllegalAccessException e) {
				throw new IllegalStateException(e);
			}
		}

		for (Entry<String, Object> e : REFERENCED_OBJECTS.entrySet()) {
			final Class<?> clazz = e.getValue().getClass();
			for (Field f : clazz.getFields()) {
				final Annotation a = f
						.getAnnotation(cspfj.util.Statistic.class);
				if (a != null) {
					try {
						digest.put(e.getKey() + "." + f.getName(),
								f.get(e.getValue()));
					} catch (IllegalAccessException e1) {
						throw new IllegalStateException(e1);
					}
				}

			}
		}
		return digest;
	}

	public static void reset() {
		REFERENCED_OBJECTS.clear();
		for (Field f : STATIC_STATISTICS) {
			System.out.println(f.getGenericType());
			if (Integer.class.equals(f.getType())
					|| Integer.TYPE.equals(f.getType())
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
