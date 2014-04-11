package abscon.instance;

import java.io.File;
import java.io.IOException;
import java.lang.reflect.Array;
import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.StringTokenizer;
import java.util.jar.JarFile;
import java.util.zip.ZipEntry;

/**
 * This class allows performing some operations based on reflection.
 */
public class ReflectionManager {

	public final static char JAR_SEPARATOR_CHAR = '/';

	public static class DigestedFields {
		private List<String> names;

		private List<Class> types;

		private List<String> values;

		public String getName(int i) {
			return names.get(i);
		}

		public Class getType(int i) {
			return types.get(i);
		}

		public String getValue(int i) {
			return values.get(i);
		}

		public int getLength() {
			return names.size();
		}

		public DigestedFields() {
			names = new LinkedList<String>();
			types = new LinkedList<Class>();
			values = new LinkedList<String>();
		}

		public void add(String name, Class type, String value, int i) {
			names.add(name);
			types.add(type);
			values.add(value);
		}
	}

	public static Class getFirstInnerClassOfInheritingFrom(Class cl, Class<?> targetClass) {
		Class[] innerClasses = cl.getDeclaredClasses();
		for (int i = 0; i < innerClasses.length; i++) {
			if (targetClass.isAssignableFrom(innerClasses[i]))
				return innerClasses[i];
		}
		return null;
	}
	

	/**
	 * Replaces all occurences of the given old char with the given new char. This method is used as the standard method of the String class do not behave correctly for some characters.
	 * 
	 * @param s a given String
	 * @param oldChar the old character
	 * @param newChar the new character
	 * @return
	 */
	private static String replaceAll(String s, char oldChar, char newChar) {
		StringBuffer sb = new StringBuffer(s);
		for (int i = 0; i < sb.length(); i++)
			if (sb.charAt(i) == oldChar)
				sb.setCharAt(i, newChar);
		return sb.toString();
	}

	/**
	 * Returns the absolute name of the given class (without the extension .class) wrt the given package name. Hence, this name starts with the given package name (and not with the root of a file
	 * system).
	 * 
	 * @param classFile a given File denoting a class.
	 * @param basicPackageName a given package name
	 */
	private static String getAbsoluteClassNameOf(File classFile, String basicPackageName) {
		String s = replaceAll(classFile.getAbsolutePath(), File.separatorChar, '.');
		int firstIndex = s.indexOf(basicPackageName);
		assert firstIndex != -1;
		int lastIndex = s.lastIndexOf(".");
		s = s.substring(firstIndex, lastIndex);
		return s;
	}

	private static void updateListIfSubclassing(List<Class> list, Class<?> rootClass, String absoluteClassName, int requiredModifiers, int forbiddenModifiers) {
		try {
			Class c = Class.forName(absoluteClassName);
			if ((c.getModifiers() & requiredModifiers) == requiredModifiers && (c.getModifiers() & forbiddenModifiers) == 0 && rootClass.isAssignableFrom(c)) {
				// if (!Modifier.isAbstract(c.getModifiers()) && rootClass.isAssignableFrom(c))
				list.add(c);
				// System.out.println("Class = " + c.getName());
			}
		} catch (ClassNotFoundException e) {
			(e.getCause() == null ? e : e.getCause()).printStackTrace();
		}
	}

	/**
	 * Returns a list of all (not abstract) classes which inherit from the given root class and which can be found from the given directory.
	 * 
	 * @param rootClass a given class
	 * @param directory a given directory
	 * @param requiredModifiers a propagationSet of required modifiers for all subclasses
	 * @param forbiddenModifiers a propagationSet of forbidden modifiers for all subclasses
	 */
	public static List<Class> searchClassesInheritingFromIn(Class rootClass, File directory, int requiredModifiers, int forbiddenModifiers) {
		assert directory.isDirectory();
		List<Class> list = new ArrayList<Class>();
		File[] files = directory.listFiles();
		for (int i = 0; i < files.length; i++) {
			if (files[i].isDirectory())
				list.addAll(searchClassesInheritingFromIn(rootClass, files[i], requiredModifiers, forbiddenModifiers));
			else if (files[i].getName().endsWith(".class"))
				updateListIfSubclassing(list, rootClass, getAbsoluteClassNameOf(files[i], rootClass.getPackage().getName()), requiredModifiers, forbiddenModifiers);
			// String absoluteClassName = getAbsoluteClassNameOf(list[i], rootClass.getPackage().getName());
		}
		return list;
	}

	public static Enumeration getEntriesOf(String jarName) {
		try {
			JarFile jf = new JarFile(jarName);
			return jf.entries();
		} catch (IOException e) {
			// Tools.dealWithFatalException(e);
		}
		return null;
	}

	/**
	 * Returns a list of all (not abstract) classes which inherit from the given root class and which can be found from the given directory.
	 * 
	 * @param rootClass a given class
	 * @param directory a given directory
	 * @param requiredModifiers a propagationSet of required modifiers for all subclasses
	 * @param forbiddenModifiers a propagationSet of forbidden modifiers for all subclasses
	 */
	private static List<Class> searchClassesInheretingFromInJar(Class rootClass, String jarName, int requiredModifiers, int forbiddenModifiers) {
		List<Class> list = new ArrayList<Class>();
		Enumeration enumeration = getEntriesOf(jarName);
		if (enumeration == null)
			return list;
		while (enumeration.hasMoreElements()) {
			String name = ((ZipEntry) enumeration.nextElement()).getName();
			String packTmp = replaceAll(rootClass.getPackage().getName(), '.', JAR_SEPARATOR_CHAR);

			if (!name.endsWith(".class") || !name.startsWith(packTmp)) // rootClass.getPackage().getName()))
				continue;
			// .class is removed and each '/' is replaced by '.' as in jar '/' is always the class separator
			name = replaceAll(name.substring(0, name.lastIndexOf(".")), JAR_SEPARATOR_CHAR, '.');
			updateListIfSubclassing(list, rootClass, name, requiredModifiers, forbiddenModifiers);
		}
		return list;
	}

	/**
	 * Returns the File object corresponding to the given directory.
	 * 
	 * @param classPathToken a given path (element of the CLASSPATH environment variable)
	 * @param clazz a given class
	 */
	private static File getDirectoryOf(String classPathToken, String basicDirectory) {
		basicDirectory = replaceAll(basicDirectory, '.', File.separatorChar);
		return new File(classPathToken + (classPathToken.endsWith(File.separator) ? "" : File.separator) + basicDirectory);
	}

	/**
	 * Returns the directory where the file corresponding to the given class may be found
	 * 
	 * @param classPathToken a given path (element of the CLASSPATH environment variable)
	 * @param clazz a given class
	 */
	private static File getDirectoryOf(String classPathToken, Class clazz) {
		return getDirectoryOf(classPathToken, clazz.getPackage().getName());
	}

	/**
	 * Returns all classes that inherit from the given root class (by considering the CLASSPATH environment variable).
	 * 
	 * @param rootClass a given class
	 * @param requiredModifiers a set of required modifiers for all subclasses
	 * @param forbiddenModifiers a set of forbidden modifiers for all subclasses
	 */
	public static Class[] searchClassesInheritingFrom(Class rootClass, int requiredModifiers, int forbiddenModifiers) {
		List<Class> classes = new ArrayList<Class>();

		StringTokenizer st = new StringTokenizer(System.getProperty("java.class.path", "."), File.pathSeparator);
		while (st.hasMoreTokens()) {
			String classPathToken = st.nextToken();
			if (classPathToken.endsWith(".jar"))
				classes.addAll(searchClassesInheretingFromInJar(rootClass, classPathToken, requiredModifiers, forbiddenModifiers));
			else {
				File file = getDirectoryOf(classPathToken, rootClass);
				if (file.exists() && file.isDirectory())
					classes.addAll(searchClassesInheritingFromIn(rootClass, file, requiredModifiers, forbiddenModifiers));
			}
		}
		return classes.toArray(new Class[0]);
	}

	public static Field getFirstFieldOfWithType(Class cl, Class<?> targetClass) {
		Field[] fields = cl.getDeclaredFields();
		for (int i = 0; i < fields.length; i++) {
			// System.out.println("filed = " + fields[i]);
			if (targetClass.isAssignableFrom(fields[i].getType()))
				return fields[i];
		}
		return (cl.getSuperclass() == null ? null : getFirstFieldOfWithType(cl.getSuperclass(), targetClass));
	}

	public static DigestedFields getAllFieldsOfFieldOfInheritingFrom(Object object, Class targetClass) {
		// Class problemClass = solver.getProblem().getClass();
		Class innerClass = ReflectionManager.getFirstInnerClassOfInheritingFrom(object.getClass(), targetClass);
		Field field = ReflectionManager.getFirstFieldOfWithType(object.getClass(), targetClass);
		try {
			field.setAccessible(true);
			Object value = field.get(object);

			DigestedFields digest = new DigestedFields();
			while (innerClass != null) {
				Field[] fields = innerClass.getDeclaredFields();
				for (int i = 0, cpt = 0; i < fields.length; i++) {
					fields[i].setAccessible(true);
					if (fields[i].getName().startsWith("this"))
						continue;
					if (fields[i].getType().getName().equals(object.getClass().getName()))
						continue; // since there exists a reference to the outer class in any inner class
					// if (fields[i].getType().isInstance(targetClass)) continue; // since there exists a reference to the outer class in any inner
					// class
					digest.add(fields[i].getName(), fields[i].getType(), fields[i].get(value).toString(), cpt++);
				}
				innerClass = innerClass.getSuperclass();
			}
			return digest;
		} catch (IllegalAccessException e) {
			(e.getCause() == null ? e : e.getCause()).printStackTrace();
		}
		return null;
	}

	/**
	 * Returns a description (names and values) of the fields of the given object.
	 */
	public static String[][] getDescriptionOfDeclaredFieldsOf(Object object) {
		String[][] description = null;
		try {
			Class cl = object.getClass();
			Field[] fields = cl.getDeclaredFields();
			// -1 since there is a field that represents the object itself (this)
			description = new String[fields.length - 1][2];
			for (int i = 0; i < fields.length - 1; i++) {
				fields[i].setAccessible(true);
				description[i][0] = fields[i].getName();
				String value = fields[i].get(object).toString();
				int position = value.lastIndexOf('/');
				value = value.substring(position + 1);
				description[i][1] = value; // fields[i].get(object).toString();
			}
		} catch (Exception e) {
			(e.getCause() == null ? e : e.getCause()).printStackTrace();
		}
		return description;
	}

	public static String getStringConcatenationOfDeclaredFieldsOf(Object object) {
		StringBuffer sb = new StringBuffer();
		try {
			Class cl = object.getClass();
			Field[] fields = cl.getDeclaredFields();
			// -1 since there is a field that represents the object itself (this)
			for (int i = 0; i < fields.length - 1; i++) {
				fields[i].setAccessible(true);
				String value = fields[i].get(object).toString();
				int position = value.lastIndexOf('/');
				value = value.substring(position + 1);
				if (i > 0)
					sb.append(" ");
				sb.append(value);
			}
		} catch (Exception e) {
			(e.getCause() == null ? e : e.getCause()).printStackTrace();
		}
		return sb.toString();
	}

	private static String searchClassInDirectory(File directory, String name) {
		File[] files = directory.listFiles();
		for (int i = 0; i < files.length; i++) {
			File entry = files[i];
			if (entry.isDirectory()) {
				String path = searchClassInDirectory(entry, name);
				if (path != null)
					return path;
			} else if (entry.getName().equals(name))
				return entry.getPath();
		}
		return null;
	}

	private static String searchClassInJar(String jarName, String basicDirectory, String className) {
		Enumeration enumeration = getEntriesOf(jarName);
		if (enumeration == null)
			return null;

		while (enumeration.hasMoreElements()) {
			String name = ((ZipEntry) enumeration.nextElement()).getName();

			// System.out.println("name = " + name);

			if (!name.startsWith(basicDirectory))
				continue;

			if (!name.substring(name.lastIndexOf('/') + 1).equals(className))
				continue;

			// System.out.println("found for " + jarName + " " + basicDirectory + " " + className + " : " + name);

			return replaceAll(name.substring(0, name.lastIndexOf(".")), JAR_SEPARATOR_CHAR, '.');
		}
		return null;
	}

	/**
	 * Returns the absolute name of the class whose name is given.
	 * 
	 * @param basicPackage the (absolute) name of a package
	 * @param className the name of a class that must be found in the package (or subpackages) whose name is given
	 */
	public static String searchAbsoluteNameOf(String basicPackage, String className) {
		StringTokenizer st = new StringTokenizer(System.getProperty("java.class.path", "."), File.pathSeparator);
		while (st.hasMoreTokens()) {
			String classPathToken = st.nextToken();
			// System.out.println("token = " + classPathToken);
			if (classPathToken.endsWith(".jar")) {
				String basicDirectory = replaceAll(basicPackage, '.', JAR_SEPARATOR_CHAR); // in jar '/' is always the class separator
				String path = searchClassInJar(classPathToken, basicDirectory, className + ".class");
				if (path != null)
					return path;
			} else {
				File directory = getDirectoryOf(classPathToken, basicPackage);
				if (directory.exists() && directory.isDirectory()) {
					String path = searchClassInDirectory(directory, className + ".class");
					if (path != null) {
						path = path.substring(classPathToken.length() + (classPathToken.endsWith(File.separator) ? 0 : 1), path.lastIndexOf("."));
						return replaceAll(path, File.separatorChar, '.');
					}
				}
			}
		}
		return null;
	}

	public static Method searchMethod(Class clazz, String methodName, int modifiers) {
		Method[] allClassMethods = clazz.getMethods();
		for (int j = 0; j < allClassMethods.length; j++) {
			if (!allClassMethods[j].getName().equals(methodName) || ((allClassMethods[j].getModifiers() & modifiers) != modifiers))
				continue;
			return allClassMethods[j];
		}
		return null;
	}

	public static Method[] searchMethods(Class[] classes, String methodName, int modifiers) {
		Method[] methods = new Method[classes.length];
		for (int i = 0; i < methods.length; i++) {
			methods[i] = searchMethod(classes[i], methodName, modifiers);
			// if (methods[i] == null)
			// System.out.println("Missing method " + methodName + " in " + classes[i].getName());
			// throw new MissingImplementationException("Missing method " + methodName + " in " + classes[i].getName());

		}
		return methods;
	}

	public static Constructor searchFirstConstructor(Class clazz, int modifiers) {
		// System.out.println("class=" + clazz.getName() + " " + clazz.getConstructors().length);
		Constructor[] constructors = clazz.getConstructors();
		Constructor constructor = null;
		for (int i = 0; i < constructors.length; i++) {
			if (((constructors[i].getModifiers() & modifiers) != modifiers))
				continue;
			// Class[] types = constructors[i].getParameterTypes();
			// if (!types[1].isArray() || types[1].getComponentType() != Variable.class)
			// continue;
			constructor = constructors[i];
		}
		return constructor;
	}

	public static Constructor[] searchFirstConstructors(Class[] classes, int modifiers) {
		Constructor[] constructors = new Constructor[classes.length];
		for (int i = 0; i < constructors.length; i++) {
			constructors[i] = searchFirstConstructor(classes[i], modifiers);
			if (constructors[i] == null)
				throw new IllegalArgumentException("Missing constructor in " + classes[i].getName());
		}
		return constructors;
	}

	public static Object getInstanceOf(Class clazz) {
		try {
			return clazz.newInstance();
		} catch (Exception e) {
			(e.getCause() == null ? e : e.getCause()).printStackTrace();
			return null;
		}
	}
	
	
	/**
	 * An object of the class whose name is given is built. Be careful: the default constructor is used.
	 * 
	 * @param className the name of a class
	 * @return an object of the class whose name is given
	 */
	public static Object getInstanceOf(String className) {
		try {
			return Class.forName(className).newInstance();
		} catch (Exception e) {
			(e.getCause() == null ? e : e.getCause()).printStackTrace();
			return null;
		}
	}

	public static Object invokeStaticMethod(String className, String methodName) {
		try {
			return Class.forName(className).getDeclaredMethod(methodName, (Class<?>)null).invoke(null, (Object[])null);
		} catch (Exception e) {
			(e.getCause() == null ? e : e.getCause()).printStackTrace();
			return null;
		}
	}

	private static Map<String, String> mapOfClassNames = new HashMap<String, String>();

	/**
	 * An object of the class whose name is given is built. There are two restrictions:
	 * <ul>
	 * <li>the class whose name is given must not be asbtract
	 * <li>the class whose name is given must inherit from the given class Be careful: the default constructor is used.
	 * 
	 * @param className the name of a class
	 * @param rootClass an ancestor class of the class whose name is given
	 * @return an object of the class whose name is given
	 */
	public static Object getInstanceOf(String className, Class rootClass) {
		String classPackageName = rootClass.getPackage().getName();
		String key = classPackageName + className;

		String absoluteClassName = mapOfClassNames.get(key);
		if (absoluteClassName == null) {
			absoluteClassName = searchAbsoluteNameOf(rootClass.getPackage().getName(), className);
			if (absoluteClassName == null)
				throw new RuntimeException("Class " + className + " not found");
			mapOfClassNames.put(key, absoluteClassName);

		}
		try {
			Class cn = Class.forName(absoluteClassName);
			Class<?> rcn = Class.forName(rootClass.getName());
			if (!rcn.isAssignableFrom(cn))
				throw new RuntimeException(absoluteClassName + " does not extend " + rootClass.getName());
			if (Modifier.isAbstract(cn.getModifiers()))
				throw new RuntimeException(className + " is abstract");
			return cn.newInstance();
		} catch (Exception e) {
			(e.getCause() == null ? e : e.getCause()).printStackTrace();
			return null;
		}
	}

	/**
	 * An object of the class whose name is given is built. The given objects ared passed to the constructor.
	 * 
	 * @param className the name of a class
	 * @param objects the parameters used by the constructor
	 * @return an object of the class whose name is given
	 */
	public static Object getInstanceOf(String className, Object[] objects) {
		try {
			Class cn = Class.forName(className);
			Constructor[] cs = cn.getConstructors();
			assert (cs.length == 1);
			return cs[0].newInstance(objects);
		} catch (Exception e) {
			(e.getCause() == null ? e : e.getCause()).printStackTrace();
			return null;
		}
	}

	/**
	 * An array with elements of the given type is built.
	 * 
	 * @param className the name of a class
	 * @param lengths the dimensions of the array
	 * @return an array
	 */
	public static Object getArrayInstance(String className, int[] lengths) {
		try {
			return Array.newInstance(Class.forName(className), lengths);
		} catch (Exception e) {
			(e.getCause() == null ? e : e.getCause()).printStackTrace();
			return null;
		}
	}

	// public static void main(String[] args) throws ClassNotFoundException {
	// Class[] classes = searchClassesInheritingFrom(Constraint.class, Modifier.PUBLIC, Modifier.ABSTRACT);
	// Method[] methods = searchMethods(classes, "getPredicateExpression", Modifier.STATIC);
	// Constructor[] constructors = Reflector.searchFirstConstructors(classes, Modifier.PUBLIC);
	// for (int i = 0; i < classes.length; i++) {
	// System.out.println(classes[i].getName() + "\n with method " + methods[i] + "\n constructor " + constructors[i]);
	// }
	// }
}