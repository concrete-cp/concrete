package cspfj.priorityqueues;

import java.util.AbstractQueue;
import java.util.Arrays;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

public class BinomialHeap<T extends Identified> extends AbstractQueue<T> {

	private final Key<T> key;

	private BinomialHeapNode<T>[] trees;

	private int size = 0;

	private int minPos = -1;

	private int minKey;

	public BinomialHeap(Key<T> key) {
		this.key = key;
		trees = (BinomialHeapNode<T>[]) new BinomialHeapNode[8];
	}

	@Override
	public Iterator<T> iterator() {
		throw new UnsupportedOperationException();
	}

	@Override
	public int size() {
		return size;
	}

	@Override
	public boolean offer(T arg0) {
		merge(new BinomialHeapNode[] { new BinomialHeapNode<T>(arg0) });
		return true;
	}

	@Override
	public T peek() {
		return trees[minPos].data;
	}

	@Override
	public T poll() {
		// TODO Auto-generated method stub
		return null;
	}

	private void merge(BinomialHeapNode<T>[] trees) {
		if (trees.length > this.trees.length) {
			this.trees = Arrays.copyOf(this.trees, trees.length);
		}
		for (int i = 0; i < trees.length; i++) {
			if (trees[i] == null) {
				continue;
			}

			final int newPos = carryMerge(trees[i], i);
			final int newKey = key.getKey(this.trees[newPos].data);
			if (minPos < 0 || newKey <= minKey) {
				minPos = newPos;
				minKey = newKey;
			}
		}
	}

	private int carryMerge(BinomialHeapNode<T> tree, int i) {
		if (i > trees.length) {
			this.trees = Arrays.copyOf(this.trees, trees.length + 1);
			trees[i] = tree;
			return i;
		}
		if (trees[i] == null) {
			trees[i] = tree;
			return i;
		}
		final BinomialHeapNode<T> newTree = mergeTree(trees[i], tree);
		this.trees[i] = null;
		return carryMerge(newTree, i + 1);

	}

	private BinomialHeapNode<T> mergeTree(BinomialHeapNode<T> p,
			BinomialHeapNode<T> q) {
		if (key.getKey(p.data) <= key.getKey(q.data)) {
			p.addSubTree(q);
			return p;
		} else {
			q.addSubTree(p);
			return q;
		}
	}

	public String toString() {
		final StringBuilder stb = new StringBuilder();
		stb.append("min: ").append(minPos).append('\n');
		for (BinomialHeapNode<T> n : trees) {
			stb.append(n).append('\n');
		}
		return stb.toString();
	}

	private static class BinomialHeapNode<T> {
		private final T data;

		private BinomialHeapNode<T> child;

		private BinomialHeapNode<T> sibling;

		private int rank;

		public BinomialHeapNode(T data) {
			this.data = data;
			child = null;
			sibling = null;
			rank = 0;
		}

		public void addSubTree(BinomialHeapNode<T> subTree) {
			if (child != null) {
				subTree.sibling = child;
			}
			rank++;
			child = subTree;
		}

		public String toString() {
			return tree(this, 0);
		}

		private static <T> String tree(BinomialHeapNode<T> node, int depth) {
			final StringBuilder stb = new StringBuilder();
			for (int i = depth; --i >= 0;) {
				stb.append("--");
			}
			stb.append(node.data).append(" (").append(node.rank).append(")\n");
			if (node.child != null) {
				stb.append(tree(node.child, depth + 1));
			}
			if (node.sibling != null) {
				stb.append(tree(node.sibling, depth));
			}
			return stb.toString();
		}
	}

}
