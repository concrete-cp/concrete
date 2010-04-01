package cspfj.constraint;

import java.util.HashMap;
import java.util.Map;

public final class ResidueManager {

    private final Map<Integer, int[]>[] last;

    public ResidueManager(final int arity) {
        last = (Map<Integer, int[]>[]) new Map<?, ?>[arity];
        for (int i = arity; --i >= 0;) {
            last[i] = new HashMap<Integer, int[]>();
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
}
