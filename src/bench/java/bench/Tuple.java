package bench;

import java.util.Arrays;

public class Tuple<T> {
    private final T[] tuple;
    private boolean unknownHashCode = true;
    private int hashCode;

    public Tuple(T[] tuple) {
        this.tuple = tuple;
    }

    public int hashCode() {
        if (unknownHashCode) {
            hashCode = Arrays.hashCode(tuple);
            unknownHashCode = false;
        }
        return hashCode;
    }

    public boolean equals(Object obj) {
        if (!(obj instanceof Tuple<?>)) {
            return false;
        }
        return Arrays.equals(tuple, ((Tuple<T>) obj).tuple);
    }
}