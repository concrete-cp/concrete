package concrete.constraint.semantic.energy;

import java.util.Optional;

public class DefaultComputeSlackStrategy implements ComputeSlackStrategy {
    public Optional<Integer> computeSlack(SlackDatastructure datastructure, int t1, int t2) {
        int slack = datastructure.querySlack(t1, t2);

        if (slack < 0 && t2 > t1) {
            return Optional.empty();
        } else {
            return Optional.of(slack);
        }
    }
}
