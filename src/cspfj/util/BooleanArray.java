package cspfj.util;

public class BooleanArray {
	private final static int[] masks;

	static {
		masks = new int[Integer.SIZE];
		for (int i = 0; i < masks.length; i++) {
			masks[i] = 0x1 << (Integer.SIZE - i - 1);
		}
	}

	private BooleanArray() {
		super();
	}

	public static int[] newBooleanArray(final int size, final boolean fill) {
		final int[] array = new int[booleanArraySize(size)];
		initBooleanArray(array, fill);

		return array;
	}

	public static void initBooleanArray(final int[] array, final boolean fill) {
		for (int i = array.length; --i >= 0;) {
			array[i] = fill ? 0xFFFFFFFF : 0;
		}
	}

	public static int booleanArraySize(final int size) {
		return 1 + size / Integer.SIZE;
	}

	public static boolean set(final int[] array, final int position,
			final boolean status) {

		int part = array[position / Integer.SIZE];
		final int i = Integer.bitCount(part);

		if (status) {
			part |= masks[position % Integer.SIZE];
		} else {
			part &= ~masks[position % Integer.SIZE];

		}

		array[position / Integer.SIZE] = part;
		return Integer.bitCount(part) != i;
	}

	public static boolean isTrue(final int[] array, final int position) {
		return (array[position / Integer.SIZE] & masks[position % Integer.SIZE]) != 0;
	}

	public static int leftMostCommonIndex(final int[] mask, final int[] domain) {
		for (int i = 0; i < mask.length; i++) {
			final int valid = mask[i] & domain[i];
			// System.err.println(Integer.toBinaryString(mask[i]) + " & "
			// + Integer.toBinaryString(domain[i]) + " = "
			// + Integer.numberOfLeadingZeros(valid));
			if (valid != 0) {
				return Integer.SIZE * i + Integer.numberOfLeadingZeros(valid);
			}

		}
		return -1;
	}

	public static String toString(int[] array) {
		final StringBuffer sb = new StringBuffer();

		for (int i = 0; i < array.length; i++) {
			sb.append(Integer.toBinaryString(array[i]));
			// sb.append(array[i]) ;
			if (i + 1 < array.length) {
				sb.append('-');
			}
		}
		return sb.toString();
	}

	public static void setSingle(int[] booleanDomain, int index) {
		initBooleanArray(booleanDomain, false);
		set(booleanDomain, index, true);
	
	}

	//
	// public static void main(final String[] args) {
	// final int[] cb = newBooleanArray(40, false);
	// final int[] mask = newBooleanArray(40, false);
	// set(cb, 7, true);
	// set(mask, 7, true);
	// set(cb, 31, true);
	// System.out.println(leftMostCommonIndex(cb, mask));
	//
	// }

}
