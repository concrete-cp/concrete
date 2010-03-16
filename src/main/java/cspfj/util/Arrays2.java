package cspfj.util;

public final class Arrays2 {
    private Arrays2() {

    }

    public static <T> T[] addBefore(T val, T[] array, T[] target) {
        assert target.length == array.length + 1;
        target[0] = val;
        System.arraycopy(array, 0, target, 1, array.length);
        return target;
    }
}
