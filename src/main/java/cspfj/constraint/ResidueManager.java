package cspfj.constraint;

public interface ResidueManager {

    public abstract int[] getResidue(final int position, final int index);

    public abstract void updateResidue(final int[] residue);

}