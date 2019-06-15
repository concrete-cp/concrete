package concrete.constraint.semantic.energy;

public class VirtualSlackDatastructure implements SlackDatastructure {
    private LogarithmicSlackDatastructure datastructure;
    private int maxLct;

    public VirtualSlackDatastructure(LogarithmicSlackDatastructure datastructure, int maxLct) {
        this.datastructure = datastructure;
        this.maxLct = maxLct;
    }

    @Override()
    public int querySlack(int l, int u) {
        int lPrime = Math.max(0, maxLct - u);
        int uPrime = maxLct - l;

        return datastructure.querySlack(lPrime, uPrime);
    }
}
