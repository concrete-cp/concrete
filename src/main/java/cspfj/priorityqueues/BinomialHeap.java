package cspfj.priorityqueues;

import java.util.AbstractQueue;
import java.util.Arrays;
import java.util.Iterator;

public final class BinomialHeap<T extends Identified> extends AbstractQueue<T> {

    private static final int DEFAULT_SIZE = 10;

    private static final float LOG2 = (float) Math.log(2);

    private final Key<T> key;

    private BinomialHeapNode<T>[] trees;

    private int size = 0;

    private BinomialHeapNode<T>[] map;

    private int iter = 0;

    public int insert = 0;
    public int update = 0;
    public int remove = 0;

    public BinomialHeap(final Key<T> key) {
        this(key, DEFAULT_SIZE);
    }

    public BinomialHeap(final Key<T> key, final int initSize) {
        this.key = key;
        map = (BinomialHeapNode<T>[]) new BinomialHeapNode[initSize];
        trees = (BinomialHeapNode<T>[]) new BinomialHeapNode[maxNbTrees(initSize)];
    }

    private static int maxNbTrees(final int size) {
        return 2 + (int) Math.floor(Math.log(size) / LOG2);
    }

    /**
     * Increases the capacity of this instance, if necessary, to ensure that it
     * can hold at least the number of elements specified by the minimum
     * capacity argument.
     * 
     * @param minCapacity
     *            the desired minimum capacity
     */
    private void ensureCapacity(final int minCapacity) {
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
    public Iterator<T> iterator() {
        throw new UnsupportedOperationException();
    }

    @Override
    public int size() {
        return size;
    }

    @Override
    public boolean offer(final T arg0) {
        final int id = arg0.getId();

        ensureCapacity(id + 1);

        BinomialHeapNode<T> node = map[id];

        if (node == null) {
            node = new BinomialHeapNode<T>(arg0);
            map[id] = node;
        }

        final float oldKey = node.key;
        final float newKey = key.getKey(arg0);
        node.key = newKey;

        if (node.inQueue == iter) {
            update++;
            if (newKey < oldKey) {
                decreaseKey(node, false);
            } else if (newKey > oldKey) {
                increaseKey(node);
            }
            return false;
        }
        insert++;
        node.clear();

        carryMerge(node, 0);
        node.inQueue = iter;
        size++;
        return true;
    }

    @Override
    public T peek() {
        return trees[minTree()].data;
    }

    @Override
    public T poll() {
        remove++;
        final BinomialHeapNode<T> min = trees[minTree()];
        deleteRoot(min);
        min.inQueue = -1;
        size--;
        return min.data;
    }

    private void deleteRoot(final BinomialHeapNode<T> root) {
        assert root.parent == null;
        trees[root.rank] = null;

        BinomialHeapNode<T> subTree = root.child;
        while (subTree != null) {
            final BinomialHeapNode<T> next = subTree.right;
            subTree.right = null;
            subTree.parent = null;

            carryMerge(subTree, subTree.rank);
            subTree = next;
        }
    }

    private int minTree() {
        int min = -1;
        float minKey = 0;
        for (int i = trees.length; --i >= 0;) {
            final BinomialHeapNode<T> tree = trees[i];
            if (tree != null && (min < 0 || tree.key <= minKey)) {
                min = i;
                minKey = tree.key;
            }
        }
        return min;
    }

    @Override
    public void clear() {
        Arrays.fill(trees, null);
        iter++;
        size = 0;
    }

    private void carryMerge(final BinomialHeapNode<T> tree, final int i) {
        final BinomialHeapNode<T> storedTree = trees[i];

        if (storedTree == null) {

            trees[i] = tree;

        } else {

            trees[i] = null;

            /**
             * We merge either the stored tree under the given tree or the
             * reverse.
             */
            if (storedTree.key <= tree.key) {
                storedTree.addSubTree(tree);
                carryMerge(storedTree, i + 1);
            } else {
                tree.addSubTree(storedTree);
                carryMerge(tree, i + 1);
            }
        }
    }

    private BinomialHeapNode<T> decreaseKey(final BinomialHeapNode<T> node,
            final boolean delete) {
        BinomialHeapNode<T> swappy;
        for (swappy = node; swappy.parent != null
                && (delete || swappy.parent.key > swappy.key); swappy = swappy.parent) {
            swap(swappy, swappy.parent);
        }
        return swappy;
    }

    private void increaseKey(final BinomialHeapNode<T> node) {
        final BinomialHeapNode<T> root = decreaseKey(node, true);
        deleteRoot(root);
        root.clear();
        carryMerge(root, 0);
    }

    @Override
    public String toString() {
        final StringBuilder stb = new StringBuilder();
        for (BinomialHeapNode<T> n : trees) {
            stb.append(n).append('\n');
        }
        return stb.toString();
    }

    private void swap(final BinomialHeapNode<T> node1,
            final BinomialHeapNode<T> node2) {
        final T node1Data = node1.data;
        final float node1Key = node1.key;
        map[node1Data.getId()] = node2;
        map[node2.data.getId()] = node1;
        node1.data = node2.data;
        node1.key = node2.key;
        node2.data = node1Data;
        node2.key = node1Key;
    }

    private static final class BinomialHeapNode<T> {
        private T data;

        private float key;

        private BinomialHeapNode<T> child;

        private BinomialHeapNode<T> right;

        private BinomialHeapNode<T> parent;

        private int rank;

        private int inQueue;

        private BinomialHeapNode(final T data) {
            this.data = data;
            inQueue = -1;
            clear();
        }

        public void clear() {
            child = null;
            right = null;
            parent = null;
            rank = 0;
        }

        public void addSubTree(final BinomialHeapNode<T> subTree) {
            if (child != null) {
                subTree.right = child;
            }
            rank++;
            child = subTree;
            subTree.parent = this;
        }

        @Override
        public String toString() {
            final StringBuilder stb = new StringBuilder();
            tree(stb, 0);
            return stb.toString();
        }

        private void tree(final StringBuilder stb, final int depth) {
            for (int i = depth; --i >= 0;) {
                stb.append("--");
            }
            stb.append(data).append(" (").append(key).append(", ").append(rank)
                    .append(")\n");
            if (child != null) {
                child.tree(stb, depth + 1);
            }
            if (right != null) {
                right.tree(stb, depth);
            }
        }
    }

}
