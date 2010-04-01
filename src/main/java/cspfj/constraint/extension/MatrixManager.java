package cspfj.constraint.extension;

public interface MatrixManager extends Cloneable {
    boolean removeTuple(int[] tuple);

    boolean check();

    boolean supportCondition(int position);

    String getType();

}