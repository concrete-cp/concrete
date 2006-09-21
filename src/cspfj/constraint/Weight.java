package cspfj.constraint;

import java.util.Comparator;


public class Weight implements Comparator<Constraint> {

    final boolean reverse;

    public Weight(final boolean reverse) {
        super();
        this.reverse = reverse;
    }

    public int compare(Constraint arg0, Constraint arg1) {
        final int compare = arg0.getWeight() - arg1.getWeight();

        if (compare == 0) {
            return arg0.getId() - arg1.getId();
        }
        return reverse ? -compare : compare;
    }

}
