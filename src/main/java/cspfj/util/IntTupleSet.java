package cspfj.util;

import java.util.AbstractSet;
import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.NoSuchElementException;

/**
 * Implements an hashtable of tuples, hopefully faster than by wrapping a tuple
 * in an object and using JDK's HashSet.
 * 
 * @author vion
 * 
 *         type of tuples
 */
public final class IntTupleSet extends AbstractSet<int[]> {

	private static final int DEFAULT_HASHTABLE_SIZE = 32;

	private static final float LOAD_FACTOR = .75f;

	private int treshold;

	private Entry[] hashTable;

	private int size;

	private int hashTableSize;

	public IntTupleSet(final int nbTuples) {
		super();
		this.size = 0;
		hashTableSize = 1;
		while (hashTableSize < nbTuples) {
			hashTableSize <<= 1;
		}
		treshold = (int) (hashTableSize * LOAD_FACTOR);
		hashTable = new Entry[hashTableSize];
	}

	public IntTupleSet() {
		this(DEFAULT_HASHTABLE_SIZE);
	}

	public IntTupleSet(final Collection<int[]> collection) {
		this(collection.size());
		addAll(collection);
	}

	/**
	 * @param hash
	 *            hash of the tuple to seek
	 * @param hashTableSize
	 *            size of the hash table
	 * @return The index of the tuple in the hashtable
	 */
	private static int indexFor(final int hash, final int hashTableSize) {
		return hash & (hashTableSize - 1);
	}

	@Override
	public boolean contains(final Object obj) {
		return containsTuple((int[]) obj);
	}

	public boolean containsTuple(final int[] tuple) {
		return containsTuple(hashTable[indexFor(Arrays.hashCode(tuple),
				hashTableSize)], tuple);
	}

	@Override
	public boolean add(final int[] tuple) {
		final int hash = Arrays.hashCode(tuple);
		final int index = indexFor(hash, hashTableSize);

		if (containsTuple(hashTable[index], tuple)) {
			return false;
		}

		hashTable[index] = new Entry(tuple, hash, hashTable[index]);
		if (size++ >= treshold) {
			resize(hashTable.length * 2);
		}
		return true;
	}

	private void resize(final int newTableSize) {
		final Entry[] newTable = new Entry[newTableSize];

		transfer(newTable);
		hashTable = newTable;
		hashTableSize = newTableSize;
		treshold = (int) (newTableSize * LOAD_FACTOR);
	}

	@Override
	public boolean remove(final Object obj) {
		final int[] tuple = (int[]) obj;
		final int index = indexFor(Arrays.hashCode(tuple), hashTableSize);
		Entry prev = hashTable[index];
		if (Arrays.equals(prev.entry, tuple)) {
			hashTable[index] = prev.next;
			size--;
			return true;
		}

		for (Entry e = prev.next; e != null; prev = e, e = e.next) {
			if (Arrays.equals(e.entry, tuple)) {
				prev.next = e.next;
				size--;
				return true;
			}
		}

		return false;
	}

	/**
	 * Transfers all entries from current table to newTable.
	 */
	private void transfer(final Entry[] newTable) {
		final Entry[] src = hashTable;
		final int newCapacity = newTable.length;
		for (int j = src.length; --j >= 0;) {
			Entry e = src[j];
			if (e != null) {
				src[j] = null;
				do {
					final Entry next = e.next;
					final int i = indexFor(e.hash, newCapacity);
					e.next = newTable[i];
					newTable[i] = e;
					e = next;
				} while (e != null);
			}
		}
	}

	@Override
	public Iterator<int[]> iterator() {
		return new HashSetIterator();
	}

	private static boolean containsTuple(Entry list, int[] tuple) {
		for (Entry e = list; e != null; e = e.next) {
			if (Arrays.equals(e.entry, tuple)) {
				return true;
			}
		}
		return false;
	}

	private class HashSetIterator implements Iterator<int[]> {

		private int index;

		private Entry next;

		public HashSetIterator() {
			if (size > 0) { // advance to first entry
				final Entry[] t = hashTable;
				while (index < t.length && (next = t[index++]) == null)
					;
			}
		}

		@Override
		public boolean hasNext() {
			return next != null;
		}

		@Override
		public int[] next() {
			Entry e = next;
			if (e == null) {
				throw new NoSuchElementException();
			}

			if ((next = e.next) == null) {
				Entry[] t = hashTable;
				while (index < t.length && (next = t[index++]) == null)
					;
			}
			return e.entry;
		}

		@Override
		public void remove() {
			throw new UnsupportedOperationException();
		}

	}

	@Override
	public int size() {
		return size;
	}

	@Override
	public void clear() {
		Arrays.fill(hashTable, null);
		size = 0;
	}

	private static class Entry {
		final int[] entry;
		Entry next;
		final int hash;

		public Entry(final int[] entry, final int hash, final Entry next) {
			this.entry = entry;
			this.hash = hash;
			this.next = next;
		}
	}
}
