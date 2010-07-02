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
	 * @return an float key used to sort given objects.
	 */
	float getKey(T object);
}
