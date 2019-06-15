package concrete.constraint.semantic.energy;

public interface NegativeIntervalEventListener {
    void fail(Task task, int t1, int t2, int slack);
}
