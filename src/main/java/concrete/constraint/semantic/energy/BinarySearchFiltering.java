package concrete.constraint.semantic.energy;


import scala.NotImplementedError;

public class BinarySearchFiltering implements NegativeIntervalEventListener {
    private Task[] tasks;
    private Task[] reverseTasks;
    private int C;
    private CumulativeArguments args;
    private int[] estPrime;
    private int[] lctPrime;
    private int maxLct;
    private boolean doReversePasses;
    private boolean fail;

    private BinarySearchChecker checker;
    private BinarySearchChecker reverseChecker;
    private LogarithmicSlackDatastructure datastructure;

    public BinarySearchFiltering(Task[] tasks, int C, CumulativeArguments args) {
        this.args = args;
        this.tasks = tasks;
        this.reverseTasks = new Task[tasks.length];
        this.C = C;
        this.doReversePasses = false;
        this.fail = false;

        maxLct = 0;
        estPrime = new int[tasks.length];
        lctPrime = new int[tasks.length];
        for (int i = 0; i < tasks.length; i++) {
            estPrime[i] = tasks[i].est();
            lctPrime[i] = tasks[i].lct();
            maxLct = Math.max(maxLct, tasks[i].lct());
        }

        for (int i = 0; i < tasks.length; i++) {
            reverseTasks[i] =
                    new Task(
                            i + 1,
                            maxLct - tasks[i].lct(),
                            maxLct - tasks[i].est(),
                            tasks[i].p(),
                            tasks[i].h());
        }

        checker = new BinarySearchChecker(tasks, C, args);
        reverseChecker = new BinarySearchChecker(reverseTasks, C, args, true);
    }

    public void update() {
        maxLct = 0;
        for (Task task : tasks) {
            maxLct = Math.max(maxLct, task.lct());
        }

        for (int i = 0; i < tasks.length; i++) {
            reverseTasks[i].est_$eq(maxLct - tasks[i].lct());
            reverseTasks[i].lct_$eq(maxLct - tasks[i].est());
        }
    }

    public FilteredBounds filter() {
        new BruteFilteringMatrix(tasks, C);
        fail = false;
        doReversePasses = false;
        for (int i = 0; i < tasks.length; i++) {
            estPrime[i] = tasks[i].est();
            lctPrime[i] = tasks[i].lct();
            maxLct = Math.max(maxLct, tasks[i].lct());
        }

        datastructure = new LogarithmicSlackDatastructure(tasks, C, args);
        VirtualSlackDatastructure virtualDatastructure =
                new VirtualSlackDatastructure(datastructure, maxLct);

        FilteringComputeSlackStrategy computeSlackStrategy;

        switch (args.numberOfPasses()) {
            case 2:
                computeSlackStrategy = new TwoPassesComputeSlackStrategy(this, true);
                break;
            case 4:
                computeSlackStrategy = new FourPassesComputeSlackStrategy(this, true);
                break;

            default:
                throw new NotImplementedError();
        }

        checker.setComputeSlackStrategy(computeSlackStrategy);
        checker.setDatastructure(datastructure);
        reverseChecker.setComputeSlackStrategy(computeSlackStrategy);
        reverseChecker.setDatastructure(virtualDatastructure);

        checker.update();
        reverseChecker.update();

        int inf = Integer.MAX_VALUE;
        int negInf = Integer.MIN_VALUE;

        for (int i = 0; i < tasks.length; i++) {
            //Filter est on O1 x O2 U Ot matrix
            computeSlackStrategy.setTaskToFilter(tasks[i]);
            computeSlackStrategy.setUseLeftShift(true);
            checker.isConsistent(negInf, tasks[i].ect(), negInf, inf);

            //Filter est on O2 U Ot x O1 matrix
            computeSlackStrategy.setTaskToFilter(reverseTasks[i]);
            computeSlackStrategy.setUseLeftShift(false);
            reverseChecker.isConsistent(maxLct - tasks[i].lct(), maxLct - tasks[i].est(), negInf, inf);

            if (fail) {
                return null;
            }
        }

        if (args.numberOfPasses() == 4) {
            doReversePasses = true;
            for (int i = 0; i < tasks.length; i++) {
                //Filter lct on O1 x O2 U Ot matrix
                computeSlackStrategy.setTaskToFilter(tasks[i]);
                computeSlackStrategy.setUseLeftShift(false);
                checker.isConsistent(tasks[i].est(), tasks[i].lct(), negInf, inf);

                //Filter lct on O2 U Ot x O1 matrix
                computeSlackStrategy.setTaskToFilter(reverseTasks[i]);
                computeSlackStrategy.setUseLeftShift(true);
                reverseChecker.isConsistent(negInf, maxLct - tasks[i].lst(), negInf, inf);

                if (fail || !derrienPart(tasks[i])) {
                    return null;
                }
            }
        }

        return new FilteredBounds(estPrime, lctPrime);
    }

    private boolean derrienPart(Task at) {
        int t1;
        int t2;
        boolean pass = true;

        // L_3
        filterForTaskAndInterval(at, at.est(), at.ect());
        // R_3
        filterForTaskAndInterval(at, at.lst(), at.lct());
        for (Task it : tasks) {
            //L_1
            t1 = it.est();
            t2 = at.ect();
            if (t2 > it.lct() && t1 < at.ect()) {
                pass &= filterForTaskAndInterval(at, t1, t2);
            }

            t2 = at.ect();
            t1 = it.est() + it.lct() - t2;
            if (t2 < it.lct() && t2 > it.lst() && t2 > it.ect() && t1 < at.ect()) {
                pass &= filterForTaskAndInterval(at, t1, t2);
            }

            t1 = it.lst();
            t2 = at.ect();
            if (t2 < it.lct() && t2 > it.lst() && t2 <= it.ect() && t1 < at.ect()) {
                pass &= filterForTaskAndInterval(at, t1, t2);
            }

            // L_2
            t1 = at.est();
            t2 = it.lct();
            if (t2 > at.est() && t1 <= it.est()) {
                pass &= filterForTaskAndInterval(at, t1, t2);
            }

            t1 = at.est();
            t2 = it.est() + it.lct() - t1;
            if (t2 > at.est() && t1 > it.est() && t1 < it.ect() && t1 < it.lst()) {
                pass &= filterForTaskAndInterval(at, t1, t2);
            }

            t1 = at.est();
            t2 = it.ect();
            if (t2 > at.est() && t1 > it.est() && t1 < it.ect() && t1 >= it.lst()) {
                pass &= filterForTaskAndInterval(at, t1, t2);
            }

            // R_1
            t1 = it.est();
            t2 = at.lct();
            if (t2 >= it.lct() && t1 < at.lct()) {
                pass &= filterForTaskAndInterval(at, t1, t2);
            }

            t2 = at.lct();
            t1 = it.est() + it.lct() - t2;
            if (t2 < it.lct() && t2 > it.lst() && t2 > it.ect() && t1 < at.lct()) {
                pass &= filterForTaskAndInterval(at, t1, t2);
            }

            t1 = it.lst();
            t2 = at.lct();
            if (t2 < it.lct() && t2 > it.lst() && t2 <= it.ect() && t1 < at.lct()) {
                pass &= filterForTaskAndInterval(at, t1, t2);
            }

            // R_2
            t1 = at.lst();
            t2 = it.lct();
            if (t2 > at.lst() && t1 <= it.est()) {
                pass &= filterForTaskAndInterval(at, t1, t2);
            }

            t1 = at.lst();
            t2 = it.est() + it.lct() - t1;
            if (t2 > at.lst() && t1 > it.est() && t1 < it.ect() && t1 < it.lst()) {
                pass &= filterForTaskAndInterval(at, t1, t2);
            }

            t1 = at.lst();
            t2 = it.ect();
            if (t2 > at.lst() && t1 > it.est() && t1 < it.ect() && t1 >= it.lst()) {
                pass &= filterForTaskAndInterval(at, t1, t2);
            }

            if (!pass) {
                return false;
            }
        }

        return true;
    }

    private boolean filterForTaskAndInterval(Task task, int t1, int t2) {
        if (t2 <= t1) {
            return true;
        }

        int slack = datastructure.querySlack(t1, t2);

        if (slack < 0) {
            return false;
        }

        int i = task.getId();
        slack += task.computeMinimumIntersection(t1, t2);
        int leftShift = task.computeLeftShift(t1, t2);
        int rightShift = task.computeRightShift(t1, t2);
        if (slack < leftShift) {
            int adjustment = (int) Math.ceil(t2 - 1.0 * slack / task.h());
            estPrime[i] = Math.max(estPrime[i], adjustment);
        }

        if (slack < rightShift) {
            int adjustment = (int) (t1 + 1.0 * slack / task.h());
            lctPrime[i] = Math.min(lctPrime[i], adjustment);
        }

        return estPrime[i] + task.p() <= lctPrime[i];
    }

    public void fail(Task task, int t1, int t2, int avail) {
        if (fail) {
            return;
        }

        boolean doReverseTasks = false;
        int i = task.getId();
        if (task != tasks[i]) {
            doReverseTasks = true;
            task = tasks[i];
            int temp = t1;
            t1 = maxLct - t2;
            t2 = maxLct - temp;
        }

        switch (args.numberOfPasses()) {
            case 2:
                filterEst(task, t1, t2, avail);
                filterLct(task, t1, t2, avail);
                break;

            case 4:
                if (!doReversePasses && !doReverseTasks) {
                    avail += task.computeLeftShift(t1, t2);
                    filterEst(task, t1, t2, avail);
                } else if (!doReversePasses && doReverseTasks) {
                    avail += tasks[i].computeLeftShift(t1, t2);
                    filterEst(task, t1, t2, avail);
                } else if (doReversePasses && !doReverseTasks) {
                    avail += task.computeRightShift(t1, t2);
                    filterLct(task, t1, t2, avail);
                } else {
                    avail += tasks[i].computeRightShift(t1, t2);
                    filterLct(task, t1, t2, avail);
                }
                break;

            default:
                throw new NotImplementedError();
        }

        if (estPrime[i] + task.p() > lctPrime[i]) {
            fail = true;
        }
    }

    public void filterEst(Task task, int t1, int t2, int avail) {
        int i = task.getId();
        int leftShift = task.computeLeftShift(t1, t2);
        if (leftShift > avail) {
            int newEst = (int) Math.ceil(t2 - (avail * 1.0) / task.h());
            estPrime[i] = Math.max(estPrime[i], newEst);
        }
    }

    public void filterLct(Task task, int t1, int t2, int avail) {
        int i = task.getId();
        int rightShift = task.computeRightShift(t1, t2);
        if (rightShift > avail) {
            int newLct = (int) Math.floor(t1 + (avail * 1.0) / task.h());
            lctPrime[i] = Math.min(lctPrime[i], newLct);
        }
    }
}
