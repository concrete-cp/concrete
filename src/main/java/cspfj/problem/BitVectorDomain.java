package cspfj.problem;

import java.util.Arrays;

import cspfj.util.BitVector;
import cspfj.util.LargeBitVector;
import cspfj.util.SmallBitVector;

public class BitVectorDomain implements Domain {

	private BitVector bvDomain;

	private int[] domain;

	private int size;

	private BitVector[] bvHistory;
	private int[] dsHistory;

	private int level;

	public BitVectorDomain(int[] domain) {
		if (domain.length > 64) {
			bvDomain = new LargeBitVector(domain.length, true);
		} else {
			bvDomain = new SmallBitVector(domain.length, true);
		}
		this.domain = domain.clone();
		size = domain.length;
		bvHistory = new BitVector[0];
		dsHistory = new int[0];
	}

	@Override
	public int first() {
		return bvDomain.nextSetBit(0);
	}

	@Override
	public int size() {
		return size;
	}

	@Override
	public int last() {
		return bvDomain.prevSetBit(domain.length);
	}

	@Override
	public int lastAbsent() {
		return bvDomain.prevClearBit(domain.length);
	}

	@Override
	public int next(int i) {
		return bvDomain.nextSetBit(i + 1);
	}

	@Override
	public int prev(int i) {
		return bvDomain.prevSetBit(i);
	}

	@Override
	public int prevAbsent(int i) {
		return bvDomain.prevClearBit(i);
	}

	/**
	 * @param value
	 *            La valeur dont on veut obtenir l'index
	 * @return L'index de la valeur donnée en paramètre
	 * @throws NotInDomainException
	 */
	public int index(final int value) {
		final int[] domain = this.domain;
		for (int i = domain.length; --i >= 0;) {
			if (domain[i] == value) {
				return i;
			}
		}

		return -1;
	}

	/**
	 * @param index
	 *            L'index à tester
	 * @return True si l'index est absent
	 */
	@Override
	public boolean present(int index) {
		return bvDomain.get(index);
	}

	@Override
	public void setSingle(int index) {
		bvDomain.setSingle(index);
		size = 1;
	}

	@Override
	public int value(int index) {
		return domain[index];
	}

	@Override
	public void remove(int index) {
		size--;
		bvDomain.clear(index);
	}

	@Override
	public IntIterator iterator() {
		return bvDomain.iterator();
	}

	@Override
	public int[] current() {
		final int[] values = new int[size];

		for (int i = first(), j = 0; i >= 0; i = next(i), j++) {
			values[j] = i;
		}

		return values;
	}

	final long[] arrayDomain = new long[1];

	public BitVector getBitVector() {
		return bvDomain;
	}

	public BitVectorDomain clone() {
		final BitVectorDomain clone;
		try {
			clone = (BitVectorDomain) super.clone();
		} catch (CloneNotSupportedException e) {
			throw new IllegalStateException(e);
		}
		clone.domain = domain.clone();
		clone.bvHistory = bvHistory.clone();
		clone.dsHistory = dsHistory.clone();
		return clone;
	}

	@Override
	public int maxSize() {
		return domain.length;
	}

	public void push() {
		if (level >= bvHistory.length) {
			bvHistory = Arrays.copyOf(bvHistory, level + 1);
			dsHistory = Arrays.copyOf(dsHistory, level + 1);
		}
		bvDomain.copyTo(bvHistory[level]);
		dsHistory[level++] = size;

	}

	public void pop() {
		bvHistory[--level].copyTo(bvDomain);
		size = dsHistory[level];
	}

	public void reset() {
		bvDomain = bvHistory[0];
		size = dsHistory[0];
		level = 0;
	}
}
