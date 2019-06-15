package concrete.constraint.semantic.energy;

import java.util.Optional;

public interface ComputeSlackStrategy {
    Optional<Integer> computeSlack(SlackDatastructure datastructure, int t1, int t2);
}
