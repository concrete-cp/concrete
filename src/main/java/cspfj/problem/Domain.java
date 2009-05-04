package cspfj.problem;

import cspfj.util.BitVector;

public interface Domain {
    int first();

    int last();

    int next(int i);

    int prev(int i);

    int lastAbsent();

    int prevAbsent(int i);

    int size();

    int index(int value);

    int value(int index);

    int maxSize();

    boolean present(int index);

    void setSingle(int index);

    void remove(int index);

    Domain clone();

    void setLevel(int level);

    void restoreLevel(int level);

    BitVector getAtLevel(int level);

    int[] allValues();
}
