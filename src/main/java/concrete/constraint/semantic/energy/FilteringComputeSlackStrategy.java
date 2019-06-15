package concrete.constraint.semantic.energy;

public abstract class FilteringComputeSlackStrategy implements ComputeSlackStrategy {
    protected NegativeIntervalEventListener listener;
    protected Task taskToFilter;
    protected boolean useLeftShift;

    public FilteringComputeSlackStrategy(NegativeIntervalEventListener listener, boolean useLeftShift) {
        this.listener = listener;
        this.useLeftShift = useLeftShift;
    }

    public void setTaskToFilter(Task task) {
        this.taskToFilter = task;
    }

    public void setUseLeftShift(boolean useLeftShift) {
        this.useLeftShift = useLeftShift;
    }
}
