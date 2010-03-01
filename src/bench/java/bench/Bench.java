package bench;

import java.util.HashSet;
import java.util.Random;

import cspfj.util.IntTupleSet;
import cspom.extension.TupleSet;

public class Bench {

    private static final int NB_ITER = 50;
    private static final int NB_TUPLES = 200000;
    private static final int ARITY = 4;

    private static final int[][] tuplesInt = new int[NB_TUPLES][ARITY];
    private static final Integer[][] tuplesInteger = new Integer[NB_TUPLES][ARITY];
    private static final Tuple<Integer>[] tuplesTuple = (Tuple<Integer>[]) new Tuple<?>[NB_TUPLES];
    static {
        final Random RAND = new Random(0);
        for (int i = NB_TUPLES; --i > 0;) {
            for (int j = ARITY; --j >= 0;) {
                final int rand = RAND.nextInt();
                tuplesInt[i][j] = rand;
                tuplesInteger[i][j] = rand;
            }
            tuplesTuple[i] = new Tuple<Integer>(tuplesInteger[i]);
        }
    }

    public static void main(String[] args) {
        //
        // final HashSet<Tuple<Integer>> hs2 = new HashSet<Tuple<Integer>>();
        // hs2.add(new Tuple<Integer>(new Integer[] { 1, 2, 3 }));
        // if (!hs2.contains(new Tuple<Integer>(new Integer[] { 1, 2, 3 }))) {
        // throw new IllegalStateException();
        // }
        long time;

        time = -System.currentTimeMillis();
        final HashSet<Tuple<Integer>> hs = new HashSet<Tuple<Integer>>();
        for (int i = NB_ITER; --i >= 0;) {
            for (Integer[] t : tuplesInteger) {
                hs.add(new Tuple<Integer>(t));
            }
        }
        time += System.currentTimeMillis();
        System.out.println("HashSet with new Tuple: " + time / 1000d);

        time = -System.currentTimeMillis();
        for (int i = NB_ITER; --i >= 0;) {
            for (Integer[] t : tuplesInteger) {
                hs.contains(new Tuple<Integer>(t));
            }
        }
        time += System.currentTimeMillis();
        System.out.println("contains: " + time / 1000d);

        time = -System.currentTimeMillis();
        final HashSet<Tuple<Integer>> hs3 = new HashSet<Tuple<Integer>>();
        for (int i = NB_ITER; --i >= 0;) {
            for (Tuple<Integer> t : tuplesTuple) {
                hs3.add(t);
            }
        }
        time += System.currentTimeMillis();
        System.out.println("HashSet with preTuple: " + time / 1000d);

        time = -System.currentTimeMillis();
        for (int i = NB_ITER; --i >= 0;) {
            for (Tuple<Integer> t : tuplesTuple) {
                hs3.contains(t);
            }
        }
        time += System.currentTimeMillis();
        System.out.println("contains: " + time / 1000d);

        /*
         * IntTupleSet
         */

        time = -System.currentTimeMillis();
        final IntTupleSet its = new IntTupleSet();
        for (int i = NB_ITER; --i >= 0;) {
            for (int[] t : tuplesInt) {
                its.add(t);
            }
        }
        time += System.currentTimeMillis();

        System.out.println("IntTupleSet: " + time / 1000d);
        time = -System.currentTimeMillis();
        for (int i = NB_ITER; --i >= 0;) {
            for (int[] t : tuplesInt) {
                its.contains(t);
            }
        }
        time += System.currentTimeMillis();
        System.out.println("contains: " + time / 1000d);

        /*
         * TupleSet
         */
        time = -System.currentTimeMillis();
        final TupleSet<Integer> ts = new TupleSet<Integer>();
        for (int i = NB_ITER; --i >= 0;) {
            for (Integer[] t : tuplesInteger) {
                ts.add(t);
            }
        }
        time += System.currentTimeMillis();
        System.out.println("TupleSet: " + time / 1000d);

        time = -System.currentTimeMillis();
        for (int i = NB_ITER; --i >= 0;) {
            for (Integer[] t : tuplesInteger) {
                ts.contains(t);
            }
        }
        time += System.currentTimeMillis();
        System.out.println("contains: " + time / 1000d);

    }
}
