package concrete.constraint.semantic.energy;

import java.util.Arrays;

public class FilteredBounds {
    public int[] est;
    public int[] lct;

    public FilteredBounds(int[] est, int[] lct) {
        this.est = est;
        this.lct = lct;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        FilteredBounds that = (FilteredBounds) o;

        return Arrays.equals(est, that.est) && Arrays.equals(lct, that.lct);
    }

    @Override
    public int hashCode() {
        int result = Arrays.hashCode(est);
        result = 31 * result + Arrays.hashCode(lct);
        return result;
    }
}
