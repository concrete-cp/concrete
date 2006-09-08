package cspfj.heuristic;
import java.util.Comparator;

import cspfj.problem.Variable;

public interface Heuristic extends Comparator<Variable> {
    Variable selectVariable() ;
    
    
}
