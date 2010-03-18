package cspfj.util;


public final class Arrays2 {

    private static final int DISPLAYED_VALUES = 5;

    private Arrays2() {

    }

    public static <T> T[] addBefore(T val, T[] array, T[] target) {
        assert target.length == array.length + 1;
        target[0] = val;
        System.arraycopy(array, 0, target, 1, array.length);
        return target;
    }

    public static <T> String partialDisplay(T[] values) {
        final int iMax = values.length - 1;
        if (iMax == -1) {
            return "[]";
        }

        final StringBuilder b = new StringBuilder();
        b.append('[');
        int max = DISPLAYED_VALUES;
        for (int i = 0;; i++) {
            b.append(String.valueOf(values[i]));
            if (i == iMax) {
                return b.append(']').toString();
            }
            if (--max == 0) {
                return b.append("... (").append(iMax + 1 - DISPLAYED_VALUES)
                        .append(" more)]").toString();
            }
            b.append(", ");
        }
    }
    
    public static String partialDisplay(int[] values) {
        final int iMax = values.length - 1;
        if (iMax == -1) {
            return "[]";
        }

        final StringBuilder b = new StringBuilder();
        b.append('[');
        int max = DISPLAYED_VALUES;
        for (int i = 0;; i++) {
            b.append(values[i]);
            if (i == iMax) {
                return b.append(']').toString();
            }
            if (--max == 0) {
                return b.append("... (").append(iMax + 1 - DISPLAYED_VALUES)
                        .append(" more)]").toString();
            }
            b.append(", ");
        }
    }
}
