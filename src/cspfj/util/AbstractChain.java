package cspfj.util;

public abstract class AbstractChain implements Cloneable {

	protected int[] next;

	protected int[] prev;

	protected int firstPresentIndex;

	protected int lastPresentIndex;

	public AbstractChain(int length) {
		super();
		next = new int[length];
		prev = new int[length];
		clear() ;
	}

	public final void clear() {
		firstPresentIndex = lastPresentIndex = -1;
	}
	
	public final int getFirst() {
		return firstPresentIndex;
	}

	public final int getLast() {
		return lastPresentIndex;
	}

	public final int getNext(final int index) {
		return next[index];
	}

	public final int getPrev(int index) {
		return prev[index];
	}

	public AbstractChain clone() {
		AbstractChain clone = null;
		try {
			clone = (AbstractChain) super.clone();
		} catch (CloneNotSupportedException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		clone.next = next.clone();
		clone.prev = prev.clone();
		return clone;
	}

}