package cspfj.filter;

import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import cspfj.constraint.Constraint;
import cspfj.problem.Problem;
import cspfj.problem.Variable;

public final class BiSAC implements Filter {

    final Problem problem;
    final AC3 ac;

    public BiSAC(final Problem problem) {
        this.problem = problem;
        this.ac = new AC3(problem);
    }

    private static final class Pair {
        private final Variable var;
        private final int val;

        private Pair(final Variable var, final int val) {
            this.var = var;
            this.val = val;
        }

        public int hashCode() {
            return 31 * var.hashCode() + val;
        }

        public boolean equals(final Object o) {
            final Pair p = (Pair) o;
            return var.equals(p.var) && val == p.val;
        }

        public String toString() {
            return "(" + var + ", " + var.getValue(val) + ")";
        }
    }

    @Override
    public boolean reduceAll() throws InterruptedException {
        boolean change;
        if (!ac.reduceAll()) {
            return false;
        }
        do {
            change = false;
            for (Variable vi : problem.getVariables()) {
                for (int a = vi.getFirst(); a >= 0; a = vi.getNext(a)) {

                    final Set<Pair> domain = new HashSet<Pair>();
                    for (Variable vj : problem.getVariables()) {
                        for (int b = vj.getFirst(); b >= 0; b = vj.getNext(b)) {

                            problem.push();
                            vj.setSingle(b);

                            if (ac.reduceAfter(vj) && vi.isPresent(a)) {
                                domain.add(new Pair(vj, b));
                            }

                            problem.pop();
                        }
                    }

                    boolean consistent = true;

                    problem.push();
                    for (Variable v : problem.getVariables()) {
                        for (int i = v.getFirst(); i >= 0; i = v.getNext(i)) {
                            if (!domain.contains(new Pair(v, i))) {
                                v.remove(i);
                            }
                        }
                        if (v.getDomainSize() == 0) {
                            consistent = false;
                            break;
                        }
                    }
                    if (consistent) {
                        consistent = ac.reduceAll();
                    }
                    problem.pop();

                    if (!consistent) {
                        vi.remove(a);
                        if (vi.getDomainSize() == 0 || !ac.reduceAfter(vi)) {
                            return false;
                        }
                        change = true;
                    }
                }
            }
            if (change && !ac.reduceAll()) {
                return false;
            }
        } while (change);

        return true;
    }

    public boolean control() throws InterruptedException {
        if (!ac.reduceAll()) {
            return false;
        }

        for (Variable vi : problem.getVariables()) {
            for (int a = vi.getFirst(); a >= 0; a = vi.getNext(a)) {

                final Set<Pair> domain = new HashSet<Pair>();
                for (Variable vj : problem.getVariables()) {
                    for (int b = vj.getFirst(); b >= 0; b = vj.getNext(b)) {

                        problem.push();
                        vj.setSingle(b);

                        if (ac.reduceAfter(vj) && vi.isPresent(a)) {
                            domain.add(new Pair(vj, b));
                        }

                        problem.pop();
                    }
                }

                problem.push();
                for (Variable v : problem.getVariables()) {
                    for (int i = v.getFirst(); i >= 0; i = v.getNext(i)) {
                        if (!domain.contains(new Pair(v, i))) {
                            v.remove(i);
                        }
                    }
                    if (v.getDomainSize() == 0) {
                        return false;
                    }
                }
                if (!ac.reduceAll()) {
                    return false;
                }
                problem.pop();
            }
        }

        return true;
    }

    @Override
    public boolean reduceAfter(Collection<Constraint> constraints) {
        throw new UnsupportedOperationException();
    }

    @Override
    public boolean reduceAfter(Variable variable) {
        throw new UnsupportedOperationException();
    }

    @Override
    public Map<String, Object> getStatistics() {
        return new HashMap<String, Object>();
    }

}
