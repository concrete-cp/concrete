package cspfj.constraint;

public interface ResidueManager {

    int[] getResidue(final int position, final int index);

    void updateResidue(final int[] residue);

    void remove(int[] tuple);

}