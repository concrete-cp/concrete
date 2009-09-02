package cspfj.priorityqueues;

/**
 * Keys are used as alternative to Comparators, but return integer based keys
 * for better sort performance when possible.
 * 
 * @author scand1sk
 * 
 * @param <T>
 */
public interface Key<T> {
	/**
	 * @param object
	 * @return an integer key used to sort given objects.
	 */
	int getKey(T object);
}
