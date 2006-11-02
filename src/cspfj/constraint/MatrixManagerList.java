package cspfj.constraint;

import cspfj.exception.MatrixTooBigException;
import cspfj.problem.Variable;

public class MatrixManagerList extends MatrixManager {

    private final int[][][] listOfSupports ;
    
    private final int arity ;
    
    private boolean supports ;
    
    public MatrixManagerList(Variable[] scope, int[] tuple) {
        super(scope, tuple);
        arity = scope.length ;
        this.listOfSupports= new int[arity][][];

    }

    @Override
    public void init(final boolean initialState) throws MatrixTooBigException {
        super.init(initialState);
        supports = !initialState ;
    }
    
    @Override
    public boolean set(final int[] tuple, final boolean status) {
        // TODO Auto-generated method stub
        return false;
    }

    @Override
    public void intersect(final Variable[] scope, final Variable[] constraintScope,
            final boolean supports, final int[][] tuples) throws MatrixTooBigException {
        
        // TODO Auto-generated method stub

    }

    @Override
    public boolean setFirstTuple(final int variablePosition, final int index) {
        // TODO Auto-generated method stub
        return false;
    }

    @Override
    public boolean next() {
        // TODO Auto-generated method stub
        return false;
    }

}
