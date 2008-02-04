package cspfj.util;

public final class OrderedChain extends AbstractChain implements Cloneable {
	private int[] next0;

	private int[] prev0;

	public OrderedChain(int length) {
		super(length);

		this.next0 = new int[length];
		this.prev0 = new int[length];

	}

	public void reOrder(final Integer[] order, final int[] removed) {
		firstPresentIndex = order[length - 1];
		lastPresentIndex = order[0];
		for (int i = length; --i > 0;) {
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

	public OrderedChain clone() throws CloneNotSupportedException {

		final OrderedChain clone = (OrderedChain) super.clone();

		clone.next0 = next0.clone();
		clone.prev0 = prev0.clone();

		return clone;

	}
	
}
