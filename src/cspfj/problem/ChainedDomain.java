package cspfj.problem;

public final class ChainedDomain implements Cloneable {
	private final int[] next0;

	private final int[] prev0;

	private final int[] next;

	private final int[] prev;

	private int firstPresentIndex;

	private int lastPresentIndex;

	public ChainedDomain(int length) {
		this.next = new int[length];
		this.prev = new int[length];

		this.next0 = new int[length];
		this.prev0 = new int[length];
	}

	public void reOrder(final int[] order, final int[] removed) {
		firstPresentIndex = order[order.length - 1];
		lastPresentIndex = order[0];
		for (int i = order.length; --i > 0;) {
			next[order[i]] = order[i - 1];
			prev[order[i - 1]] = order[i];
		}
		prev[firstPresentIndex] = next[lastPresentIndex] = -1;

		System.arraycopy(prev, 0, prev0, 0, prev.length);
		System.arraycopy(next, 0, next0, 0, next.length);

		for (int i = removed.length; --i >= 0;) {
			if (removed[i] >= 0) {
				remove(i);
			}
		}
	}

	public void remove(final int index) {
		final int next = this.next[index];
		final int prev = this.prev[index];
		if (prev == -1) {
			firstPresentIndex = next;
		} else {
			this.next[prev] = next;
		}
		if (next == -1) {
			lastPresentIndex = prev;
		} else {
			this.prev[next] = prev;
		}
	}

	public void restore(final int index, final int[] removed) {
		int next = -1;

		for (int i = next0[index]; next == -1 && i != -1; i = next0[i]) {
			if (removed[i] == -1) {
				next = i;
			}
		}

		int prev = -1;
		for (int i = prev0[index]; prev == -1 && i != -1; i = prev0[i]) {
			if (removed[i] == -1) {
				prev = i;
			}
		}

		this.prev[index] = prev;
		this.next[index] = next;

		if (prev == -1) {
			firstPresentIndex = index;
		} else {
			this.next[prev] = index;
		}
		if (next == -1) {
			lastPresentIndex = index;
		} else {
			this.prev[next] = index;
		}
	}

	public int getFirst() {
		return firstPresentIndex;
	}

	public int getLast() {
		return lastPresentIndex;
	}

	public int getNext(final int index) {
		return next[index];
	}

	public ChainedDomain clone() {
		final int length = next.length;
		final ChainedDomain clone = new ChainedDomain(length);

		clone.firstPresentIndex = firstPresentIndex;
		clone.lastPresentIndex = lastPresentIndex;
		System.arraycopy(next, 0, clone.next, 0, length);
		System.arraycopy(prev, 0, clone.prev, 0, length);
		System.arraycopy(next0, 0, clone.next0, 0, length);
		System.arraycopy(prev0, 0, clone.prev0, 0, length);

		return clone;

	}
	//	
	// final int length = next.length;
	// final ChainedDomain domain = new ChainedDomain(length);
	//
	// domain.firstPresentIndex = firstPresentIndex;

	// return domain;
	// }
}
