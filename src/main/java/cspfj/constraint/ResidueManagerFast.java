package cspfj.constraint;

import java.util.Arrays;

import cspfj.problem.Variable;

public final class ResidueManagerFast implements ResidueManager {

    private final int[][][] last;

    public ResidueManagerFast(final Variable[] scope) {
        last = new int[scope.length][][];
        for (int i = scope.length; --i >= 0;) {
            last[i] = new int[scope[i].domain().lastIndex() + 1][];
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see cspfj.constraint.ResidueManager#getResidue(int, int)
     */
    @Override
    public int[] getResidue(final int position, final int index) {
        return last[position][index];
    }

    /*
     * (non-Javadoc)
     * 
     * @see cspfj.constraint.ResidueManager#updateResidue(int[])
     */
    @Override
    public void updateResidue(final int[] residue) {
        for (int position = last.length; --position >= 0;) {
            last[position][residue[position]] = residue;
        }
    }

    @Override
    public void remove(final int[] residue) {
        for (int position = last.length; --position >= 0;) {
            if (Arrays.equals(residue, last[position][residue[position]])) {
                last[position][residue[position]] = null;
            }
        }
    }
}
