package rb.randomlists;

public enum Structure {
    /**
     * Constant used to denote that generated lists have no structure, i.e., are
     * purely random.
     */
    UNSTRUCTURED,
    /**
     * Constant used to denote that each value must occur at least in one tuple
     * of the generated lists.
     */
    CONNECTED,

    /**
     * Constant used to denote that generated lists are balanced, i.e., all
     * values have the same number of occurrences in the different tuples of the
     * generated lists.
     */
    BALANCED
}
