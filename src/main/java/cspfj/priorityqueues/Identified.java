package cspfj.priorityqueues;

/**
 * Identified objects are object for which an unique integer identifier can be
 * obtained quickly. Identifiers should be generated upon object construction,
 * be consecutive and starting at 0. This is used to replace time-consuming
 * hashes and maps by arrays.
 * 
 * @author scand1sk
 */
public interface Identified {

	/**
	 * @return the unique, integer identifier of the object.
	 */
	int getId();
}
