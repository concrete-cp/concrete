package cspfj.constraint;

import static com.google.common.collect.Maps.newHashMap;

import java.util.Arrays;
import java.util.Map;

public final class ResidueManagerMap implements ResidueManager {

    private final Map<Integer, int[]>[] last;

    @SuppressWarnings("unchecked")
    public ResidueManagerMap(final int arity) {
        last = (Map<Integer, int[]>[]) new Map<?, ?>[arity];
        for (int i = arity; --i >= 0;) {
            last[i] = newHashMap();
        }
    }

    public int[] getResidue(final int position, final int index) {
        return last[position].get(index);
    }

    public void updateResidue(final int[] residue) {
        for (int position = last.length; --position >= 0;) {
            last[position].put(residue[position], residue);
        }
    }

    public void remove(final int[] residue) {
        for (int position = last.length; --position >= 0;) {
            if (Arrays.equals(residue, last[position].get(residue[position]))) {
                last[position].remove(residue[position]);
            }
        }
    }
}
