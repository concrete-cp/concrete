package cspfj.constraint;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

public class TupleListCraftedSet implements Matrix, Cloneable {

	// private Map<Long, Collection<int[]>> list;

	private final boolean initialContent;

	private final Cell[] hashTable;

	// //private int size;

	public static int collisions = 0;

	public static int tuples = 0;
	
	private final int hashTableSize ;

	private final static int[] primes55 = { 2, 3, 5, 7, 11, 13, 17, 19, 23, 29,
			31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101,
			103, 107, 109, 113, 127, 131, 137, 139, 149, 151, 157, 163, 167,
			173, 179, 181, 191, 193, 197, 199, 211, 223, 227, 229, 233, 239,
			241, 251, 257 };

	public TupleListCraftedSet(final int[] sizes, final boolean initialContent,
			final int nbTuples) {
		this.initialContent = initialContent;
		// list = new HashMap<Long, Collection<int[]>>();
		// this.size = 0;
		hashTableSize = firstPrimeAfter(nbTuples*4/3);
		hashTable = new Cell[hashTableSize];
		
	}

	private static int firstPrimeAfter(int value) {
		int test = value - 1;
		while (!isPrime(++test))
			;
		return test;
	}

	private static boolean isPrime(int value) {
		for (int prime : primes55) {
			if (value % prime == 0) {
				if (value == prime) {
					return true;
				} else {
					return false;
				}
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

	private static int hash(final int[] tuple) {
		int hash = -2128831035;
		for (int i : tuple) {
			hash *= 16777619;
			hash ^= i;

		}
		return hash<0?(-hash)^0x1:hash;
	}

	@Override
	public boolean check(final int[] tuple) {
		final Cell cell = hashTable[hash(tuple) % hashTableSize];
		return cell == null ? initialContent : cell.contains(tuple)
				^ initialContent;
	}

	@Override
	public void set(final int[] tuple, final boolean status) {
		if (status == initialContent) {
			hashTable[hash(tuple) % hashTableSize] = hashTable[hash(tuple)
					% hashTableSize].remove(tuple);
		} else {
			Cell cell = hashTable[hash(tuple) % hashTableSize];
			if (cell == null) {
				cell = new Cell(tuple.clone());
				tuples++;
				// size++;
			} else {
//				if (!cell.contains(tuple)) {
					cell = cell.add(tuple.clone());
					// size++;
					tuples++;
					collisions++;
//				}
			}
			hashTable[hash(tuple) % hashTableSize] = cell;
		}
	}

	public TupleListCraftedSet clone() throws CloneNotSupportedException {
		final TupleListCraftedSet list = (TupleListCraftedSet) super.clone();

		// list.list = new HashSet<BigInteger>(this.list);

		return list;
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

	public static void main(String[] args) {
		final Map<Integer, Integer> hashs = new HashMap<Integer, Integer>();
		int collisions = 0;
		for (int i = 0; i < 30; i++) {
			for (int j = 0; j < 20; j++) {
				for (int k = 0; k < 20; k++) {
					final int key = hash(new int[] { i, j, k })
							% firstPrimeAfter((int) (20 * 20 * 30));
					if (!hashs.containsKey(key)) {
						hashs.put(key, 1);
					} else {
						collisions++;
						hashs.put(key, hashs.get(key) + 1);
					}
				}
			}

		}
		System.out.println(collisions);
		for (int i : hashs.keySet()) {
			System.out.print(i + ":" + hashs.get(i) + " ");
		}
		System.out.println();

		Cell cell = new Cell(new int[] { 1, 1, 1 });

		System.out.println(cell);

		cell = cell.add(new int[] { 2, 2, 2 });

		System.out.println(cell);

		cell = cell.add(new int[] { 3, 3, 3 });
		cell = cell.add(new int[] { 4, 4, 4 });
		cell = cell.add(new int[] { 5, 5, 5 });
		cell = cell.add(new int[] { 6, 6, 6 });
		System.out.println(cell);

		System.out.println(cell.contains(new int[] { 3, 3, 3 }));
		System.out.println(cell.contains(new int[] { 2, 3, 3 }));

		cell = cell.remove(new int[] { 3, 3, 3 });
		System.out.println(cell);
		cell = cell.remove(new int[] { 1, 1, 1 });
		System.out.println(cell);
		cell = cell.remove(new int[] { 6, 6, 6 });
		System.out.println(cell);

		System.out.println(Integer.toBinaryString(-2128831035>>31));
		int i = (int) 2166136261l;
		System.out.println(i>>31);
	}

}
