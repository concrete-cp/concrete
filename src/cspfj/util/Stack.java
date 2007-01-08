package cspfj.util;

public class Stack implements Cloneable {

	protected int[] next;

	protected int firstPresentIndex;

	public Stack(int length) {
		super();
		next = new int[length];
		clear();
	}

	public final void clear() {
		firstPresentIndex = -1;
	}

	public Stack clone() {
		Stack clone = null;
		try {
			clone = (Stack) super.clone();
		} catch (CloneNotSupportedException e) {
			e.printStackTrace();
		}
		clone.next = next.clone();
		return clone;
	}

	public int pull() {
		final int index = firstPresentIndex;
		if (index < 0) {
			return -1;
		}
		firstPresentIndex = next[firstPresentIndex];
		return index;
	}

	public void push(final int index) {
		next[index] = firstPresentIndex;
		firstPresentIndex = index;
	}

	public void fill() {
		final int last = next.length - 1;
		final int[] next = this.next;
		next[last] = -1;
		for (int i = last; --i >= 0;) {
			next[i] = i + 1;
		}
		firstPresentIndex = 0;
	}
}