package cspfj.constraint;

import java.util.Comparator;

public final class Weight implements Comparator<Constraint> {

    public Weight() {
        super();
    }

    public int compare(final Constraint arg0, final Constraint arg1) {
        final int compare = Double.compare(arg0.getWeight(), arg1.getWeight());

        if (compare == 0) {
            return arg0.getId() - arg1.getId();
        }

        return compare;
    }

}
