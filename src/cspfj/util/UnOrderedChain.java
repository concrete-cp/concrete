package cspfj.util;

public final class UnOrderedChain extends AbstractChain {
	public UnOrderedChain(int length) {
		super(length);
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

	public void add(final int index) {
		prev[index] = lastPresentIndex;
		next[index] = -1;
		if (firstPresentIndex == -1) {
			firstPresentIndex = index;
		} else {
			next[lastPresentIndex] = index;
		}
		lastPresentIndex = index;
	}

}
