package cspfj.constraint.extension;

public interface MatrixManager extends Cloneable {

    void setTuple(int[] tuple);

    boolean removeTuple(int[] tuple);

    boolean check();

    boolean supportCondition(int position);

    String getType();

}