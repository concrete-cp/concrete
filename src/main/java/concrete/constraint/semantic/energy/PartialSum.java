package concrete.constraint.semantic.energy;

import java.util.Arrays;

public class PartialSum {
    private Tuple[] sortedTuples;
    private int[] times;
    private int[] sum;
    private int[] slopes;
    private int n;
    private int times_length;

    public PartialSum(Tuple[] tuples) {
        n = tuples.length + 2; // We add two sentinel values (0, 0), (maxValue * 2, 0)
        Arrays.sort(tuples);
        sortedTuples = new Tuple[n];
        System.arraycopy(tuples, 0, sortedTuples, 1, n - 2);

        sortedTuples[0] = new Tuple(0, 0);
        sortedTuples[n-1] = new Tuple(Integer.MAX_VALUE, 0);

        times = new int[n];
        sum = new int[n];
        slopes = new int[n];
        times_length = 0;
        initializeStructure();

        for (int i = 0; i < n-1; i++)
            assert sortedTuples[i].position <= sortedTuples[i+1].position;
    }

    public int computePartialSum(int lower, int upper) {
        if (sortedTuples.length == 2) // There are only sentinel values
            return 0;
        int j = PartialSum.binarySearch(times, times_length, upper);
        int i = PartialSum.binarySearch(times, times_length, lower);
        return sum[j] + (slopes[j]) * (upper - times[j]) - (sum[i] + (slopes[i]) * (lower - times[i]));
    }

    private static int binarySearch(int[] array, int n, int value) {
        assert value < array[n-1];
        assert array[0] == 0;
        for (int i = 0; i < n-1; i++)
            assert(array[i] < array[i+1]); //All elements are distincts
        int l = 0;
        int u = n - 1;
        int i = (u+l) / 2;

        while (l <= u) {
            if (array[i+1] > value && array[i] <= value) {
                return i;
            }
            if (array[i] > value) {
                u = i - 1;
            } else {
                l = i + 1;
            }
            i = (u+l) / 2;
        }
        assert false;
        return -1;
    }

    private void initializeStructure() {
        int i = 0, j = 0, c = 0, tOld = 0, cOld = 0;
        while (j < n) {
            int t = sortedTuples[j].position;
            sum[i] = cOld + (t - tOld) * c;
            times[i] = t;
            while (j < n && sortedTuples[j].position == t) {
                c += sortedTuples[j].value;
                j++;
            }
            slopes[i] = c;
            tOld = t;
            cOld = sum[i];
            i++;
        }
        times_length = i;
    }

    public static class Tuple implements Comparable<Tuple> {
        public int position;
        public int value;

        public Tuple(int position, int value) {
            this.position = position;
            this.value = value;
        }

        @Override
        public int compareTo(Tuple o) {
            return Integer.compare(this.position, o.position);
        }
    }
}
