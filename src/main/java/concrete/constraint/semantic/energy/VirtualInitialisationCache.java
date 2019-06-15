package concrete.constraint.semantic.energy;

public class VirtualInitialisationCache {
    private int[] T;
    private int[] a;
    private int[] b;
    private int cpt;

    public VirtualInitialisationCache(int maxValue) {
        T = new int[maxValue+1];
        a = new int[maxValue+1];
        b = new int[maxValue+1];
        cpt = -1;
    }

    public void reinitialise() {
        cpt = -1;
    }

    public boolean containsKey(int k) {
        return b[k] <= cpt && a[b[k]] == k;
    }

    public int get(int k) {
        assert (b[k] <= cpt && a[b[k]] == k);
        return T[k];
    }

    public void put(int k, int value) {
        T[k] = value;
        cpt += 1;
        a[cpt] = k;
        b[k] = cpt;
    }
}
