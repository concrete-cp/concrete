package generator;

/**
 * This is the pseudo-random generator proposed by P. Bratley, B.L. Fox & L.E.
 * Schrage, "A guide to Simulation", Springer Verlag, New-York, 1983. <br>
 * It is used in order to reproduce instances proposed by E. Taillard,
 * "Benchmarks for Basic Scheduling Problems", 1989.
 * 
 * @author Julien Vion
 */
public final class BratleyRandomizer {
    private long xn;

    private final static int A = 16807;

    private final static int B = 127773;

    private final static int C = 2836;

    private final static int M = 2147483647; // 2�� - 1

    public BratleyRandomizer(long seed) {
        this.reSeed(seed);
    }

    public void reSeed(final long seed) {
        if (!(0 < seed && seed < M)) {
            throw new IllegalArgumentException("0 < seed < 2�� - 1");
        }
        this.xn = seed;
    }

    public double getU01() {
        final int k = (int) Math.floor((double) this.xn / (double) B);
        long x_n1 = A * (this.xn % B) - k * C;
        if (x_n1 < 0) {
            x_n1 += M;
        }
        this.xn = x_n1;
        return (double) x_n1 / (double) M;
    }

    public int getU(final int lb, final int hb) {
        if (lb > hb) {
            throw new IllegalArgumentException("U[a,b] : a <= b (" + lb + ","
                    + hb + ")");
        }
        return (int) Math.floor((double) lb + this.getU01()
                * (double) (hb - lb + 1));
    }
}