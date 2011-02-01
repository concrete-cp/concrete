package cspfj.util;

public final class Arrays2 {

	private static final int DISPLAYED_VALUES = 5;

	private Arrays2() {

	}

	public static <T> String partialDisplay(final T[] values) {
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

	public static String partialDisplay(final int[] values) {
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

	public static boolean isOrdered(final int[] array) {
		for (int i = array.length - 1; --i >= 0;) {
			if (array[i + 1] <= array[i]) {
				return false;
			}
		}
		return true;
	}
}
