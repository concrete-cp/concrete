package concrete.constraint.semantic.energy;

import concrete.constraint.semantic.energy.Task;

import java.util.Arrays;
import java.util.Optional;

@SuppressWarnings("Duplicates")
public class BinarySearchChecker {
    private Task[] tasks;
    private int C;
    private SlackDatastructure datastructure;

    private int[] setO1;
    private int[] setO2;
    private int[] minimumIndexO2;

    private int o1Size;
    private int maxUpper;
    private int minLower;
    private int n;

    private IntervalChain chain;

    private CumulativeArguments args;
    private ComputeSlackStrategy computeSlackStrategy;

    private boolean includesOnlyOt;

    private static final int INF = Integer.MAX_VALUE;
    private static final int NEG_INF = 0;

    public BinarySearchChecker(Task[] tasks, int C, CumulativeArguments args, boolean includesOnlyOt) {
        this(tasks, C, args, new DefaultComputeSlackStrategy(), new LogarithmicSlackDatastructure(tasks, C, args), includesOnlyOt);
    }

    public BinarySearchChecker(Task[] tasks, int C, CumulativeArguments args) {
        this(tasks, C, args, new DefaultComputeSlackStrategy(), new LogarithmicSlackDatastructure(tasks, C, args), false);
    }

    public BinarySearchChecker(Task[] tasks, int C, CumulativeArguments args,
                               ComputeSlackStrategy computeSlackStrategy,
                               SlackDatastructure datastructure, boolean includesOnlyOt) {
        this.tasks = tasks;
        this.C = C;
        this.computeSlackStrategy = computeSlackStrategy;
        this.datastructure = datastructure;
        this.args = args;
        this.includesOnlyOt = includesOnlyOt;

        this.n = tasks.length;


        this.setO1 = new int[2 * n * n];
        this.setO2 = new int[2 * n * n];

        this.minimumIndexO2 = new int[2 * n * n];

        o1Size = 0;

        update();
    }

    public void update() {
        int h = 0;
        int k = 0;

        this.setO1 = new int[2 * n * n];
        this.setO2 = new int[2 * n * n];

        for (int i = 0; i < 2 * n * n; i++) {
            setO1[i] = setO2[i] = -1;
        }

        for (int i = 0; i < n; i++) {
            for (int j = 0; j < n; j++) {
                //Filter for set OC. Page 60 of Derrien's thesis
                //"Interval d'intérêts de Derrien pour le checker" in manuscript notes
                // There are few errors in Lemma 4.3 in Derrien's thesis, so it is better to use manuscript notes

                Task it = tasks[i];
                Task jt = tasks[j];

                int t1 = it.est();
                int t2 = jt.lct();
                if (t1 <= jt.est() && t2 >= it.lct() && !includesOnlyOt) {
                    setO1[h++] = t1;
                    setO2[k++] = t2;
                }

                t1 = it.lst();
                t2 = jt.lct();
                if (t1 <= jt.est() && t2 < it.lct() && t2 > it.lst() && t2 <= it.ect() && !includesOnlyOt) {
                    setO1[h++] = t1;
                    setO2[k++] = t2;
                }

                t1 = it.est();
                t2 = jt.est() + jt.lct() - t1;
                if (t1 > jt.est() && t1 < jt.ect() && t1 < jt.lst() && t2 > it.lct()) {
                    setO1[h++] = t1;
                    setO2[k++] = t2;
                }

                t1 = it.lst();
                t2 = jt.est() + jt.lct() - t1;
                if (t1 > jt.est() && t1 < jt.ect() && t1 < jt.lst() && t2 < it.lct() && t2 > it.lst() && t2 <= it.ect()) {
                    setO1[h++] = t1;
                    setO2[k++] = t2;
                }

                t1 = it.est();
                t2 = jt.lct();
                if (t1 > jt.est() && t1 < jt.ect() && t1 >= jt.lst() && t2 >= it.lct() && !includesOnlyOt) {
                    setO1[h++] = t1;
                    setO2[k++] = t2;
                }

                t1 = it.lst();
                t2 = jt.ect();
                if (t1 > jt.est() && t1 < jt.ect() && t1 >= jt.lst() && t2 < it.lct() && t2 > it.lst() && t2 <= it.ect() && !includesOnlyOt) {
                    setO1[h++] = t1;
                    setO2[k++] = t2;
                }
            }
        }

        Arrays.sort(setO1);
        Arrays.sort(setO2);

        setO1 = removeDuplicates(setO1);
        setO2 = removeDuplicates(setO2);

        if (setO2.length == 0) {
            return;
        }

        /*
        count += 1;
        sumO1 += setO1.length;
        sumO2 += setO2.length;
        maxO1 = Math.max(maxO1, setO1.length);
        maxO2 = Math.max(maxO2, setO2.length);
        */

        int j = 0;
        k = 0;
        for (int i = 0; i < setO1.length; i++) {
            while (j < setO2.length && setO1[i] >= setO2[j]) {
                j++;
            }
            minimumIndexO2[i] = Math.min(j, setO2.length - 1);
        }

        maxUpper = setO2[setO2.length - 1];
        minLower = setO2[0];
        minLower = Math.max(0, minLower);
    }

    private int[] removeDuplicates(int[] set) {
        int size = 1;

        int start = 0;
        while (start < set.length && set[start] == -1) {
            start++;
        }

        if (start == set.length) {
            return new int[0];
        }

        for (int i = start + 1; i < set.length; i++) {
            if (set[i] != set[i - 1])
                size++;
        }

        int[] temp = new int[size];
        temp[0] = set[start];
        int k = 1;

        for (int i = start + 1; i < set.length; i++) {
            if (k < size && set[i] != set[i - 1]) {
                temp[k++] = set[i];
            }
        }

        return temp;
    }

    private int findUpperInO2(int lowerIndex) throws InconsistentException {
        int l = setO1[lowerIndex];
        int left = minimumIndexO2[lowerIndex];
        int right = this.setO2.length;

        int middle = -1;
        int min = -1;

        while (left < right) {
            middle = (left + right) / 2;
            int u = setO2[middle];
            int newValue = computeSlack(l, u).orElseThrow(InconsistentException::new);

            int lStar = setO1[chain.findAssociatedLower(u)];
            int oldMin = computeSlack(lStar, u).orElseThrow(InconsistentException::new);

            if (newValue < oldMin) {
                left = middle + 1;
                min = middle;
            } else {
                right = middle;
            }
        }

        return min;
    }

    private int intervalSearch(int lowerIndex) throws InconsistentException {
        int l = setO1[lowerIndex];
        DoubleEndedStack stack = chain.getStack();

        int u = Math.max(setO2[minimumIndexO2[lowerIndex]], stack.topL());
        int lStar = setO1[stack.topLValue()];
        int i = stack.lSize();

        if (computeSlack(l, u).orElseThrow(InconsistentException::new) <= computeSlack(lStar, u).orElseThrow(InconsistentException::new)) {
            while (i < stack.size() &&
                    computeSlack(l, stack.getKey(i)).orElseThrow(InconsistentException::new) <=
                            computeSlack(setO1[stack.getValue(i - 1)], stack.getKey(i)).orElseThrow(InconsistentException::new)) {
                i++;
            }
            lStar = setO1[stack.getValue(i - 1)];

            int left = stack.getIndex(i - 1);
            int right = i < stack.size() ? stack.getIndex(i) + 1 : setO2.length;

            int middle = -1;
            int min = -1;

            while (left < right) {
                middle = (left + right) / 2;
                u = setO2[middle];
                int newValue = computeSlack(l, u).orElseThrow(InconsistentException::new);
                int oldMin = computeSlack(lStar, u).orElseThrow(InconsistentException::new);

                if (newValue <= oldMin) {
                    left = middle + 1;
                    min = middle;
                } else {
                    right = middle;
                }
            }

            //The if is not necessary since l >= topL
            if (min >= 0 && setO2[min] > l) {
                for (int j = 0; j < stack.lSize() - i; j++) {
                    stack.popU();
                }
            }

            return min;
        }

        return -1;
    }

    public boolean isConsistent() {
        return isConsistent(Integer.MIN_VALUE, Integer.MAX_VALUE, Integer.MIN_VALUE, Integer.MAX_VALUE);
    }

    public boolean isConsistent(int minO1, int maxO1, int minO2, int maxO2) {

        if (setO2.length == 0) {
            return true;
        }

        //new BruteMatrix(tasks, C).printMatrix();
        chain = new IntervalChain(minLower, maxUpper, setO2.length - 1, 0, setO1.length);

        try {
            for (int i = 1; i < setO1.length && setO1[i] <= maxO1; i++) {
                if (setO1[i] < minO1) {
                    continue;
                }

                int l = setO1[i];
                int indexO2 = intervalSearch(i);
                if (indexO2 == -1)
                    continue;

                int max = indexO2 >= 0 ? setO2[indexO2] : -1;
                int c = setO2[minimumIndexO2[i]];
                if (max > 0 && max - c > 0) {
                    chain.replace(c, max, minimumIndexO2[i], indexO2, i);
                }
            }
        } catch (InconsistentException e) {
            return false;
        }

        int lowerIndexO2 = 0;
        IntervalChain.Interval current = chain.next();
        while (current != null) {
            int t1 = setO1[current.value];

            if (t1 <= maxO1) {
                int i;
                for (i = lowerIndexO2; i < setO2.length && setO2[i] < current.u; i++) {
                    int t2 = setO2[i];
                    if (t2 > t1) {
                        Optional<Integer> r = computeSlack(t1, t2);
                        if (!r.isPresent() || r.get() < 0) {
                            return false;
                        }
                    }

                }
                lowerIndexO2 = i;
            }

            current = chain.next();
        }

        return true;
    }

    private int findIndexInO2(int value, boolean findGreater) {
        int left = 0;
        int right = setO2.length;
        int middle = -1;

        while (left < right) {
            middle = (left + right) / 2;
            if (setO2[middle] == value) {
                return middle;
            }
            if (value < setO2[middle]) {
                right = middle;
            } else {
                left = middle + 1;
            }
        }

        if (findGreater && value > setO2[middle])
            return middle + 1;
        return middle;
    }

    private Optional<Integer> computeSlack(int t1, int t2) {
        return computeSlackStrategy.computeSlack(datastructure, t1, t2);
    }

    public void setComputeSlackStrategy(ComputeSlackStrategy strategy) {
        this.computeSlackStrategy = strategy;
    }

    public void setDatastructure(SlackDatastructure datastructure) {
        this.datastructure = datastructure;
    }

    public void printMatrix() {
        System.out.println();
        System.out.printf("     |");

        for (int t2 : setO2) {
            System.out.printf("%-5d|", t2);
        }
        System.out.println();
        for (int t1 : setO1) {
            System.out.printf("%-5d|", t1);
            for (int t2 : setO2) {
                if (t2 > t1) {
                    System.out.printf("%-5d|", computeSlack(t1, t2).get());
                } else {
                    System.out.printf("%-5s|", "-");
                }
            }
            System.out.println();
        }
        System.out.println();
    }
}
