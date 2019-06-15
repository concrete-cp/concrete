package concrete.constraint.semantic.energy;

import java.util.Arrays;
import java.util.Comparator;
import java.util.HashMap;
import java.util.concurrent.Semaphore;

@SuppressWarnings("Duplicates")
public class BruteFilteringMatrix {
    private Task[] tasks;
    private int C;
    private int n;
    private int[] setO1;
    private Pair[] setO2;
    public static HashMap<Integer, HashMap<Integer, Integer>> histogram = new HashMap<>();
    private static Semaphore semaphore = new Semaphore(1);

    public BruteFilteringMatrix(Task[] tasks, int C) {
        this.tasks = tasks;
        this.C = C;
        this.n = tasks.length;

        try {
            semaphore.acquire();
            makeSets();
        } catch (InterruptedException ignored) {
        } finally {
            semaphore.release();
        }
    }

    public int querySlack(int t1, int t2) {
        int available = (t2 - t1) * C;

        int slack = available - queryConsumed(t1, t2) - computeMaximumDelta(t1, t2);
        //return 1 * (queryConsumed(t1, t2) + computeMaximumDelta(t1, t2));
        return  -1 * slack;
    }

    public void printMatrix() {
        System.out.printf("%-5s|", "j ");
        for (int i = 0; i < setO2.length; i++) {
            System.out.printf("%-5s", i);
        }
        System.out.println();

        System.out.printf("%-5s|", "t1");
        for (Pair pair : setO2) {
            System.out.printf("%-5s", pair.t1);
        }
        System.out.println();

        System.out.printf("%-5s|", "");
        for (Pair pair : setO2) {
            System.out.printf("%-5d", pair.t);
        }
        System.out.println();

        System.out.print("-----");
        for (Pair pair : setO2) {
            System.out.print("-----");
        }
        System.out.println();

        for (int t1 : setO1) {
            printRow(t1);
        }
    }

    public void printRow(int t1) {
        System.out.printf("%-5d|", t1);
        for (Pair pair : setO2) {
            int t2 = pair.t;
            if (false && t1 >= t2) {
                System.out.printf("%-5s", "N");
            } else {
                System.out.printf("%-5d", querySlack(t1, t2), pair.set);
            }
        }
        System.out.println();
    }

    public boolean isTotallyMonotone() {
        for (int i = 0; i < setO1.length - 1; i++) {
            for (int j = 0; j < setO2.length - 1; j++) {
                int a11 = querySlack(setO1[i], setO2[j].t);
                int a12 = querySlack(setO1[i], setO2[j+1].t);
                int a21 = querySlack(setO1[i+1], setO2[j].t);
                int a22 = querySlack(setO1[i+1], setO2[j+1].t);
                if (a12 < a11 && a21 < a22) {
                    // If the minimum of the rows of the 2x2 submatrix are on the
                    // top right and bottom left cells, the matrix is not totally monotone.
                    System.out.printf("The submatrix %d %d %d %d is not totally monotone. \n", i, i+1, j, j+1);
                    return false;
                }
            }
        }
        return true;
    }

    public boolean isMonotone() {
        int prevCol = 0;
        for (int i = 0; i < setO1.length; i++) {
            int min = Integer.MAX_VALUE;
            int firstMinCol = -1;
            int lastMinCol = -1;
            for (int j = 0; j < setO2.length; j++) {
                int slack = querySlack(setO1[i], setO2[j].t);
                if (slack < min) {
                    min = slack;
                    firstMinCol = lastMinCol = j;
                } else if (slack == min) {
                    lastMinCol = j;
                }
            }
            if (lastMinCol < prevCol) {
                System.out.printf("The matrix is not monotone, line %d\n", i);
                return false;
            }
            prevCol = firstMinCol;
        }

        return true;
    }

    private int queryConsumed(int t1, int t2) {
        int sum = 0;
        for (int i = 0; i < n; i++) {
            sum += minimumIntersection(i, t1, t2);
        }

        return sum;
    }

    private int computeMaximumDelta(int t1, int t2) {
        int max = 0;
        for (int i = 0; i < n; i++) {
            int delta = leftShift(i, t1, t2) - minimumIntersection(i, t1, t2);
            max = Math.max(max, delta);
        }
        return max;
    }

    private int leftShift(int i, int t1, int t2) {
        Task task = tasks[i];
        return task.h() * Math.max(0, min(
                t2 - t1,
                task.p(),
                task.ect() - t1));
    }

    private int minimumIntersection(int i, int t1, int t2) {
        Task task = tasks[i];
        return task.h() * Math.max(0, min(
                t2 - t1,
                task.p(),
                task.ect() - t1,
                t2 - task.lst()));
    }

    private int min(int... elems) {
        assert elems.length > 1;

        int min = Integer.MAX_VALUE;
        for (int elem : elems) {
            min = Math.min(min, elem);
        }

        return min;
    }

    private void makeSets() {
        setO1 = new int[n * 3];
        setO2 = new Pair[n * 3 + 3 * n * n];

        for (int i = 0; i < n; i++) {
            Task task = tasks[i];
            int position = i * 3;
            setO1[position] = task.est();
            setO1[position + 1] = task.lst();
            setO1[position + 2] = task.ect();

            setO2[position] = new Pair(task.lst(), -1, Pair.O2);
            setO2[position + 1] = new Pair(task.ect(), -1, Pair.O2);
            setO2[position + 2] = new Pair(task.lct(), -1, Pair.O2);
        }

        int k = n * 3;
        for (int t : setO1) {
            for (int i = 0; i < n; i++) {
                Task task = tasks[i];
                setO2[k] = new Pair(task.est() + task.lct() - t, t, Pair.OT);
                k++;
            }
        }

        assert k == n * 3 + 3 * n * n;

        Arrays.sort(setO1);
        Arrays.sort(setO2, Comparator.comparingInt((Pair p) -> p.t));

        for (int i = 0; i < n - 1; i++)
            assert (setO1[i] <= setO1[i + 1]);

        int count = 0;
        for (int value : setO1) {
            int total = 0;
            for (int i = 0; i < setO2.length; i++) {
                if (i > 0 && setO2[i].t == setO2[i - 1].t) {
                    continue;
                }
                if (setO2[i].set.equals(Pair.O2)) {
                    //System.out.println(count);
                    if (!histogram.containsKey(n)) {
                        histogram.put(n, new HashMap<>());
                    }
                    if (!histogram.get(n).containsKey(count)) {
                        histogram.get(n).put(count, 0);
                    }
                    histogram.get(n).put(count, histogram.get(n).get(count) + 1);
                    count = 0;
                } else if (setO2[i].t1 == value) {
                    count++;
                    total++;
                }
            }
            //System.out.println(total);
        }
    }

    private static class Pair {
        public int t;
        public int t1;
        public String set;
        public static final String O2 = "O2";
        public static final String OT = "OT";

        public Pair(int t, int t1, String set) {
            this.t = t;
            this.t1 = t1;
            this.set = set;
        }
    }
}
