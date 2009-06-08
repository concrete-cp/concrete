package cspfj.constraint.extension;

import java.math.BigInteger;
import java.util.Arrays;
import java.util.Iterator;

public class TupleSet implements Matrix, Cloneable, Iterable<int[]> {

	// private Map<Long, Collection<int[]>> list;

	private int[][][] hashTable;

	private int[] sizes;

	private int size;

	static int collisions = 0;

	static int tuples = 0;

	private int hashTableSize;

	private final boolean initialContent;

	public TupleSet(final int nbTuples, boolean initialContent) {
		super();
		// list = new HashMap<Long, Collection<int[]>>();
		this.size = 0;
		hashTableSize = firstPrimeAfter(nbTuples * 4 / 3);
		hashTable = new int[hashTableSize][][];
		sizes = new int[hashTableSize];
		this.initialContent = initialContent;
		// System.out.println("Hashtable of " + hashTableSize + " buckets");

	}

	// private static int firstPrimeAfter(final int value) {
	// int test = value;
	// while (!isPrime(test)) {
	// test++;
	// }
	// return test;
	// }

	public TupleSet(boolean initialContent) {
		this(16, initialContent);
	}

	private static int firstPrimeAfter(final int value) {
		int test = 1;
		while (test < value) {
			test <<= 1;
		}
		return test;
	}

	private static int hash(final int[] tuple, final int length) {
		final int prime = 16777619;
		int hash = 0x811c9dc5;

		for (int i : tuple) {
			hash *= prime;
			hash ^= i;
		}

		return hash & (length - 1);

	}

	public boolean containsTuple(final int[] tuple) {
		final int hash = hash(tuple, hashTableSize);
		final int[][] subList = hashTable[hash];
		if (subList == null) {
			return false;
		}
		return index(subList, sizes[hash], tuple) >= 0;
	}

	private static int index(int[][] list, int size, int[] tuple) {
		for (int i = size; --i >= 0;) {
			if (Arrays.equals(list[i], tuple)) {
				return i;
			}
		}
		return -1;
	}

	public boolean removeTuple(final int[] tuple) {
		final int hash = hash(tuple, hashTableSize);

		if (sizes[hash] == 0) {
			return false;
		}

		final int index = index(hashTable[hash], sizes[hash], tuple);
		if (index < 0) {
			return false;
		}
		remove(hash, index);
		return true;
	}

	private void remove(int hash, int index) {
		int numMoved = sizes[hash] - index - 1;
		if (numMoved > 0)
			System.arraycopy(hashTable[hash], index + 1, hashTable[hash],
					index, numMoved);
		size--;
		sizes[hash]--;
	}

	private static void ensureCapacity(int[][][] hashTable, int position,
			int minCapacity) {
		int oldCapacity = hashTable[position].length;
		if (minCapacity > oldCapacity) {
			int newCapacity = (oldCapacity * 3) / 2 + 1;
			if (newCapacity < minCapacity)
				newCapacity = minCapacity;
			// minCapacity is usually close to size, so this is a win:
			hashTable[position] = Arrays.copyOf(hashTable[position],
					newCapacity);
		}
	}

	public void add(final int[] tuple) {
		if (size >= hashTable.length) {
			expand();
		}
		final int hash = hash(tuple, hashTableSize);

		if (hashTable[hash] == null) {
			hashTable[hash] = new int[3][];
		} else {
			collisions++;
		}

		ensureCapacity(hashTable, hash, sizes[hash] + 1);

		hashTable[hash][sizes[hash]++] = tuple;
		size++;
		tuples++;
	}

	private void expand() {
		final int newTableSize = (hashTable.length * 3 + 1) / 2;
		// System.out.println("Expanding from " + hashTableSize + " to "
		// + newTableSize);
		final int[][][] newTable = new int[newTableSize][][];
		final int[] newSizes = new int[newTableSize];
		for (int[] t : this) {
			final int hash = hash(t, newTableSize);
			if (newTable[hash] == null) {
				newTable[hash] = new int[3][];
			}

			ensureCapacity(newTable, hash, newSizes[hash] + 1);
			newTable[hash][newSizes[hash]++] = t;
		}
		hashTable = newTable;
		hashTableSize = newTableSize;
		sizes = newSizes;
	}

	@Override
	public Iterator<int[]> iterator() {
		return new HashSetIterator();
	}

	private class HashSetIterator implements Iterator<int[]> {

		private int pos;

		private int lastPos;

		private int subPos;

		private int lastSubPos;

		public HashSetIterator() {
			pos = 0;
			lastPos = 0;
			subPos = 0;
			lastSubPos = -1;
			while (pos < hashTableSize && sizes[pos] <= 0) {
				pos++;
			}
		}

		@Override
		public boolean hasNext() {
			return pos < hashTableSize;
		}

		@Override
		public int[] next() {
			final int[] tuple = hashTable[pos][subPos];
			lastSubPos = subPos++;
			lastPos = pos;
			if (subPos >= sizes[pos]) {
				subPos = 0;
				do {
					pos++;
				} while (pos < hashTableSize && sizes[pos] <= 0);
			}
			return tuple;
		}

		@Override
		public void remove() {
			if (lastSubPos < 0) {
				throw new IllegalStateException();
			}
			TupleSet.this.remove(lastPos, lastSubPos);
			lastSubPos = -1;
		}

	}

	public int size() {
		return size;
	}

	public TupleSet clone() {
		final TupleSet clone;
		try {
			clone = (TupleSet) super.clone();
		} catch (CloneNotSupportedException e) {
			throw new InternalError(e.toString());
		}
		clone.sizes = this.sizes.clone();
		clone.hashTable = new int[hashTableSize][][];
		for (int i = hashTableSize; --i >= 0;) {
			if (hashTable[i] != null) {
				clone.hashTable[i] = hashTable[i].clone();
			}
		}
		return clone;
	}

	@Override
	public boolean check(int[] tuple) {
		return containsTuple(tuple) ^ initialContent;
	}

	@Override
	public void set(int[] tuple, boolean status) {
		if (status == initialContent) {
			removeTuple(tuple);
		} else {
			add(tuple.clone());
		}
	}

	public static boolean isBetterThanMatrix(final int[] sizes,
			final int nbTuples) {
		BigInteger size = BigInteger.ONE;
		for (int s : sizes) {
			size = size.multiply(BigInteger.valueOf(s));
		}

		return size.compareTo(BigInteger.valueOf(Integer.MAX_VALUE)) >= 0
				|| (4 * nbTuples < size.intValue());
	}

	public boolean getInitialContent() {
		return initialContent;
	}
}
