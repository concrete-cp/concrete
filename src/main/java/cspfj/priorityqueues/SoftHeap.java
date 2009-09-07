package cspfj.priorityqueues;

import java.util.AbstractQueue;
import java.util.Arrays;
import java.util.Iterator;

public final class SoftHeap<T extends Identified> extends AbstractQueue<T> {

	private int size = 0;

	// private final static double ERROR_RATE = .3;
	// .3 is the error rate
	private final static int R = (int) Math
			.ceil(Math.log(1 / .0005) / Math.log(2)) + 5;

	private final Key<T> key;

	private TreeNode<T>[] trees;

	private Entry<T>[] map;

	public SoftHeap(Key<T> key) {
		this.key = key;
		trees = (TreeNode<T>[]) new TreeNode[10];
		map = (Entry<T>[]) new Entry[10];

	}

	@Override
	public Iterator<T> iterator() {
		throw new UnsupportedOperationException();
	}

	@Override
	public int size() {
		return size;
	}
	private static int maxNbTrees(int size) {
		return 45;
	}
	private void ensureCapacity(int minCapacity) {
		int oldCapacity = map.length;
		if (minCapacity > oldCapacity) {
			final int newCapacity = Math.max(minCapacity,
					(oldCapacity * 3) / 2 + 1);
			// minCapacity is usually close to size, so this is a win:
			map = Arrays.copyOf(map, newCapacity);
			final int nbTrees = maxNbTrees(newCapacity);
			if (nbTrees > trees.length) {
				trees = Arrays.copyOf(trees, nbTrees);
			}
		}
	}

	@Override
	public boolean offer(T elt) {
		final int id = elt.getId();
		ensureCapacity(id + 1);
		Entry<T> e = map[id];
		if (e == null) {
			e = new Entry<T>(elt);
			map[id] = e;
		}

		if (e.inQueue) {
			return false;
		}

		e.next = null;
		e.inQueue = true;

		TreeNode<T> node = new TreeNode<T>(0, 0, e, key.getKey(elt));
		carryMerge(node, 0);
		size++;
		return true;
	}

	private void carryMerge(TreeNode<T> tree, int rank) {

		final TreeNode<T> storedTree = trees[rank];

		if (storedTree == null) {
			trees[rank] = tree;
		} else {
			trees[rank] = null;
			carryMerge(combine(storedTree, tree), rank + 1);
		}

	}

	@Override
	public T peek() {
		return trees[minTree()].first.data;
	}

	@Override
	public T poll() {
		final int minIndex = minTree();
		final TreeNode<T> min = trees[minIndex];
		final Entry<T> elt = min.first;
		min.first = min.first.next;
		min.nbEntries--;
		if (min.nbEntries <= min.size / 2) {
			if (!min.leaf) {
				min.sift();
			} else if (min.nbEntries == 0) {
				trees[minIndex] = null;
			}
		}

		size--;
		elt.inQueue = false;
		return elt.data;
	}

	public TreeNode<T> combine(TreeNode<T> tree1, TreeNode<T> tree2) {
		assert tree1.rank == tree2.rank;
		final TreeNode<T> combined = new TreeNode<T>(tree1.rank + 1, tree1.size);
		combined.left = tree1;
		combined.right = tree2;
		combined.leaf = false;
		combined.sift();
		return combined;
	}

	public String toString() {
		final StringBuilder stb = new StringBuilder();
		for (TreeNode<T> t : trees) {
			treeString(stb, t, 0);
			stb.append('\n');
		}
		return stb.toString();
	}

	public static <T> void treeString(StringBuilder stb, TreeNode<T> root,
			int level) {

		if (root == null) {
			return;
		}
		for (int i = level; --i >= 0;) {
			stb.append("-");
		}
		stb.append(root).append("\n");
		treeString(stb, root.left, level + 1);
		treeString(stb, root.right, level + 1);
	}

	private int minTree() {
		int min = -1;
		int minKey = 0;
		for (int i = trees.length; --i >= 0;) {
			final TreeNode<T> tree = trees[i];
			if (tree != null && (min < 0 || tree.cKey <= minKey)) {
				min = i;
				minKey = tree.cKey;
			}
		}
		return min;
	}

	private static final class TreeNode<T> {
		private TreeNode<T> left = null;
		private TreeNode<T> right = null;
		private Entry<T> first = null;
		private Entry<T> last = null;
		private int nbEntries = 0;
		private int cKey = -1;
		private final int rank;
		private final int size;
		private boolean leaf = true;

		public TreeNode(int rank, int previousSize) {
			this.rank = rank;
			if (rank <= R) {
				size = 1;
			} else {
				size = (3 * previousSize + 1) / 2;
			}
		}

		public TreeNode(int rank, int previousSize, Entry<T> elt, int key) {
			this(rank, previousSize);
			first = last = elt;
			cKey = key;
			nbEntries = 1;
		}

		public void sift() {
			while (nbEntries < size && !leaf) {
				if (left == null || (right != null && left.cKey > right.cKey)) {
					final TreeNode<T> temp = right;
					right = left;
					left = temp;
				}

				concatenate(left);
				cKey = left.cKey;
				left.clear();
				if (left.leaf) {
					left = null;
					if (right == null) {
						leaf = true;
					}
				} else {
					left.sift();
				}
			}
		}

		public void clear() {
			first = null;
			last = null;
			nbEntries = 0;
		}

		public void concatenate(TreeNode<T> e) {
			if (first == null) {
				last = e.last;
			} else {
				e.last.next = first;
			}
			first = e.first;
			nbEntries += e.nbEntries;

		}

		public String toString() {
			final StringBuilder stb = new StringBuilder();

			stb.append(cKey).append(": {");
			for (Entry<T> e = first; e != null; e = e.next) {
				stb.append(e).append(", ");
			}
			return stb.append("}").toString();
		}

	}

	private static final class Entry<T> {
		private final T data;
		private Entry<T> next;
		private boolean inQueue;

		public Entry(T data) {
			this.data = data;
		}

		public String toString() {
			return data.toString();
		}
	}
}
