package concrete.constraint.semantic.energy;

public class BinarySearchPropagator {
    private CumulativeArguments args;
    private Instance instance;
    private BinarySearchChecker positiveChecker;
    private BinarySearchChecker negativeChecker;
    private BinarySearchFiltering algo;

    public BinarySearchPropagator(CumulativeArguments args) {
        this.args = args;
    }

    public void update() {
        positiveChecker.update();
        negativeChecker.update();
        algo.update();
    }

    public void initialize(Instance positiveInstance, Instance negativeInstance) {
        this.instance = positiveInstance;
        this.positiveChecker = new BinarySearchChecker(positiveInstance.tasks(), positiveInstance.capa(), args);
        this.negativeChecker = new BinarySearchChecker(negativeInstance.tasks(), negativeInstance.capa(), args);
        this.algo = new BinarySearchFiltering(positiveInstance.tasks(), positiveInstance.capa(), args);
    }

    public boolean isConsistent() {
        args.virtualCache().reinitialise();
        boolean isConsistent = positiveChecker.isConsistent();
        args.virtualCache().reinitialise();
        isConsistent &= negativeChecker.isConsistent();
        args.virtualCache().reinitialise();

        return isConsistent;
    }

    public FilteredBounds filter() {
        Task[] tasks = instance.tasks();
        int n = tasks.length;
        int[] oldEst = new int[n];
        int[] oldLct = new int[n];
        FilteredBounds bounds = null;

        for (int i = 0; i < n; i++) {
            oldEst[i] = tasks[i].est();
            oldLct[i] = tasks[i].lct();
        }

        boolean changed = true;
        while (changed) {
            changed = false;
            bounds = algo.filter();
            if (bounds == null || bounds.est == null || bounds.lct == null) {
                for (int i = 0; i < n; i++) {
                    tasks[i].est_$eq(oldEst[i]);
                    tasks[i].lct_$eq(oldLct[i]);
                }
                return null;
            }

            for (int i = 0; i < n; i++) {
                if (bounds.est[i] != tasks[i].est()) {
                    assert bounds.est[i] > tasks[i].est();
                    tasks[i].est_$eq(bounds.est[i]);
                    changed = true;
                }

                if (bounds.lct[i] != tasks[i].lct()) {
                    assert bounds.lct[i] < tasks[i].lct();
                    tasks[i].lct_$eq(bounds.lct[i]);
                    changed = true;
                }
            }
            algo.update();
        }

        for (int i = 0; i < n; i++) {
            tasks[i].est_$eq(oldEst[i]);
            tasks[i].lct_$eq(oldLct[i]);
        }

        return bounds;
    }
}
