package cspfj.constraint;

import java.util.AbstractSet;
import java.util.Arrays;
import java.util.Iterator;

public class TupleSet extends AbstractSet<int[]> implements Cloneable {

	// private Map<Long, Collection<int[]>> list;

	private Cell[] hashTable;

	private int size;

	public static int collisions = 0;

	public static int tuples = 0;

	private int hashTableSize;

	private final static int[] primes55 = { 2, 3, 5, 7, 11, 13, 17, 19, 23, 29,
			31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101,
			103, 107, 109, 113, 127, 131, 137, 139, 149, 151, 157, 163, 167,
			173, 179, 181, 191, 193, 197, 199, 211, 223, 227, 229, 233, 239,
			241, 251, 257 };

	public TupleSet(final int nbTuples) {
		// list = new HashMap<Long, Collection<int[]>>();
		this.size = 0;
		hashTableSize = firstPrimeAfter(nbTuples * 4 / 3);
		hashTable = new Cell[hashTableSize];
		// System.out.println("Hashtable of " + hashTableSize + " buckets");

	}

	private static int firstPrimeAfter(final int value) {
		int test = value;
		while (!isPrime(test)) {
			test++;
		}
		return test;
	}

	private static boolean isPrime(final int value) {
		for (int prime : primes55) {
			if (value % prime == 0) {
				return value == prime;
			}
		}

		final int maxtest = value / 4;

		for (int i = 259; i < maxtest; i += 2) {
			if (value % i == 0) {
				return false;
			}
		}

		return true;

	}

	private static int hash(final int[] tuple, final int prime) {
		int hash = -2128831035;
		for (int i : tuple) {
			hash *= 16777619;
			hash ^= i;

		}
		hash %= prime;
		return Math.abs(hash);
		//		
		// long res = (hash & 0x7fffffff) | (hash & 0x80000000l);
		// return (int)(res % prime);
	}

	public boolean containsTuple(final int[] tuple) {
		final Cell cell = hashTable[hash(tuple, hashTableSize)];
		return cell == null ? false : cell.contains(tuple);
	}

	public void removeTuple(final int[] tuple) {
		final Cell cell = hashTable[hash(tuple, hashTableSize)];
		if (cell == null) {
			return ;
		}
		cell.remove(tuple);

	}

	public boolean add(final int[] tuple) {
		if (size >= hashTable.length) {
			expand();
		}
		final int position = hash(tuple, hashTableSize);

		Cell cell = hashTable[position];
		if (cell == null) {
			cell = new Cell(tuple);
			tuples++;
			size++;
		} else {
			cell = cell.add(tuple);
			size++;
			tuples++;
			collisions++;
		}
		hashTable[position] = cell;
		return true;
	}

	private void expand() {
		final int newTableSize = (hashTable.length * 3 + 1) / 2;
		// System.out.println("Expanding from " + hashTableSize + " to "
		// + newTableSize);
		final Cell[] newTable = new Cell[newTableSize];
		for (int[] t : this) {
			final int position = hash(t, newTableSize);
			Cell cell = newTable[position];
			if (cell == null) {
				cell = new Cell(t);

			} else {
				cell = cell.add(t);

			}
			newTable[position] = cell;
		}
		hashTable = newTable;
		hashTableSize = newTableSize;
	}

	@Override
	public Iterator<int[]> iterator() {
		return new HashSetIterator();
	}

	private class HashSetIterator implements Iterator<int[]> {

		private int pos;
		private Cell cell;

		public HashSetIterator() {
			pos = 0;
			cell = null;
			for (pos = 0; pos < hashTableSize; pos++) {
				if (hashTable[pos] != null) {
					cell = hashTable[pos];
					break;
				}
			}
		}

		@Override
		public boolean hasNext() {
			return pos < hashTableSize;
		}

		@Override
		public int[] next() {
			final int[] tuple = cell.tuple;
			cell = cell.next;

			if (cell == null) {
				for (pos++; pos < hashTableSize; pos++) {
					if (hashTable[pos] != null) {
						cell = hashTable[pos];
						break;
					}
				}
			}
			return tuple;
		}

		@Override
		public void remove() {
			throw new IllegalArgumentException();
		}

	}

	private static class Cell {
		private final int[] tuple;
		private Cell next;

		public Cell(final int[] tuple) {
			this.tuple = tuple;
			this.next = null;
		}

		public Cell add(final int[] tuple) {
			final Cell cell = new Cell(tuple);
			cell.next = this;
			return cell;
		}

		public boolean contains(final int[] tuple) {
			if (Arrays.equals(this.tuple, tuple)) {
				return true;
			}
			if (next == null) {
				return false;
			}
			return next.contains(tuple);
		}

		public Cell remove(final int[] tuple) {
			if (Arrays.equals(this.tuple, tuple)) {
				return next;
			}
			next = next.remove(tuple);
			return this;
		}

		public String toString() {
			if (next == null) {
				return Arrays.toString(tuple);
			}
			return next.toString() + "," + Arrays.toString(tuple);
		}

	}

	@Override
	public int size() {
		return size;
	}
	
	public TupleSet clone() throws CloneNotSupportedException {
		final TupleSet ts = (TupleSet)super.clone();
		return ts;
	}
}
