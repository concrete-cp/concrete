package cspfj.util;

import java.util.Arrays;

public class BooleanArray {
	private final static int[] MASKS;

	private final static int SIZE=Integer.SIZE;
	
	static {
		MASKS = new int[SIZE];
		for (int i = MASKS.length; --i >= 0;) {
			MASKS[i] = 0x1 << (SIZE - i - 1);
		}
	}

	private BooleanArray() {
		super();
	}

	public static int[] newBooleanArray(final int size, final boolean fill) {
		final int[] array = new int[booleanArraySize(size)];
		initBooleanArray(array, size, fill);

		return array;
	}

	public static void initBooleanArray(final int[] array, final int size,
			final boolean fill) {
		Arrays.fill(array, fill ? 0xFFFFFFFF : 0);
		array[array.length - 1] <<= SIZE - (size % SIZE);
	}

	public static int booleanArraySize(final int size) {
		return size / SIZE + ((size % SIZE) > 0 ? 1 : 0);
	}

	public static boolean set(final int[] array, final int position,
			final boolean status) {

		int part = array[position / SIZE];
		final int oldPart = part;

		if (status) {
			part |= MASKS[position % SIZE];
		} else {
			part &= ~MASKS[position % SIZE];

		}

		array[position / SIZE] = part;
		return part != oldPart;
	}

	public static boolean isTrue(final int[] array, final int position) {
		return (array[position / SIZE] & MASKS[position % SIZE]) != 0;
	}


	public static String toString(final int[] array) {
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

	public static void setSingle(final int[] booleanDomain, final int index) {
		Arrays.fill(booleanDomain, 0);
		set(booleanDomain, index, true);
	}

	public static int getPart(final int i) {
		return i / Integer.SIZE;
	}

	//
	// public static void main(final String[] args) {
	// final int[] cb = newBooleanArray(37, true);
	//
	// System.out.println(toString(cb));
	//
	// final int[] mask = newBooleanArray(40, false);
	// set(cb, 7, true);
	// set(mask, 7, true);
	// set(cb, 31, true);
	// System.out.println(leftMostCommonIndex(cb, mask));
	//
	// }

}
