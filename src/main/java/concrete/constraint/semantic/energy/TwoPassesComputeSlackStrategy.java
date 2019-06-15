package concrete.constraint.semantic.energy;


import java.util.Optional;

public class TwoPassesComputeSlackStrategy extends FilteringComputeSlackStrategy {
    public TwoPassesComputeSlackStrategy(NegativeIntervalEventListener listener, boolean useLeftShift) {
        super(listener, useLeftShift);
    }

    public Optional<Integer> computeSlack(SlackDatastructure datastructure, int t1, int t2) {
        int slack = datastructure.querySlack(t1, t2);

        slack += taskToFilter.computeMinimumIntersection(t1, t2);
        listener.fail(taskToFilter, t1, t2, slack);

        return Optional.of(slack);
    }
}
