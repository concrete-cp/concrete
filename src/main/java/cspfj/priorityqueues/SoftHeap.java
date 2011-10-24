package cspfj.priorityqueues;

import java.util.AbstractQueue;
import java.util.Arrays;
import java.util.Iterator;

public final class SoftHeap<T extends Identified> extends AbstractQueue<T> {

    private static final float ERROR_RATE = .1f;
    // .3 is the error rate
    private static final int R = (int) Math.ceil(Math.log(1 / ERROR_RATE)
            / Math.log(2)) + 5;

    private static int[] sizes;

    private static final int DEFAULT_INIT_SIZE = 10;

    static {
        sizes = new int[] { 1 };
    }

    private final Key<T> key;

    private Tree<T> first;

    private int size = 0;

    private Entry<T>[] map;

    private int iter = 0;

    public SoftHeap(final Key<T> key) {
        this(key, DEFAULT_INIT_SIZE);
    }

    @SuppressWarnings("unchecked")
    public SoftHeap(final Key<T> key, final int initSize) {
        this.key = key;
        first = null;
        map = (Entry<T>[]) new Entry[initSize];
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
    public void clear() {
        size = 0;
        first = null;
        iter++;
    }

    private static int size(final int rank) {
        if (rank <= R) {
            return 1;
        }
        final int target = rank - R;
        int oldCapacity = sizes.length;

        if (target >= oldCapacity) {
            final int newCapacity = Math.max(target + 1,
                    (oldCapacity * 3) / 2 + 1);
            sizes = Arrays.copyOf(sizes, newCapacity);
            for (int i = oldCapacity; i < newCapacity; i++) {
                sizes[i] = (3 * sizes[i - 1] + 1) / 2;
            }
        }
        return sizes[target];
    }

    private void ensureCapacity(final int minCapacity) {
        int oldCapacity = map.length;
        if (minCapacity > oldCapacity) {
            final int newCapacity = Math.max(minCapacity,
                    (oldCapacity * 3) / 2 + 1);
            // minCapacity is usually close to size, so this is a win:
            map = Arrays.copyOf(map, newCapacity);
        }
    }

    @Override
    public boolean offer(final T elt) {
        final int id = elt.getId();
        ensureCapacity(id + 1);
        Entry<T> e = map[id];
        if (e == null) {
            e = new Entry<T>(elt);
            map[id] = e;
        }

        if (e.inQueue == iter) {
            return false;
        }

        e.next = null;
        e.inQueue = iter;
        insertTree(new Tree<T>(new TreeNode<T>(e, key.getKey(elt))));
        size++;
        return true;
    }

    private void removeTree(final Tree<T> tree) {
        if (tree.prev == null) {
            first = tree.next;
        } else {
            tree.prev.next = tree.next;
        }
        if (tree.next != null) {
            tree.next.prev = tree.prev;
        }
    }

    private void insertTree(final Tree<T> tree) {
        if (first != null) {
            tree.next = first;
            first.prev = tree;
        }
        first = tree;
        while (tree.next != null && tree.rank() == tree.next.rank()) {
            tree.root = new TreeNode<T>(tree.root, tree.next.root);
            removeTree(tree.next);
        }
        tree.updateSufMin();
    }

    @Override
    public T peek() {
        return first.sufMin.root.first.data;
    }

    @Override
    public T poll() {
        final Tree<T> minTree = first.sufMin;

        final TreeNode<T> min = minTree.root;

        final double oldCKey = min.cKey;
        final Entry<T> elt = min.pick();
        if (min.nbEntries == 0) {
            removeTree(minTree);
            if (minTree.prev != null) {
                minTree.prev.updateSufMin();
            }
        } else if (oldCKey != min.cKey) {
            minTree.updateSufMin();
        }
        size--;
        elt.inQueue = -1;
        return elt.data;
    }

    public String toString() {
        final StringBuilder stb = new StringBuilder();
        for (Tree<T> t = first; t != null; t = t.next) {
            stb.append(t).append('\n');
        }
        return stb.toString();
    }

    private static final class Tree<T> {
        private TreeNode<T> root;
        private Tree<T> prev;
        private Tree<T> next;
        private Tree<T> sufMin = this;

        private Tree(final TreeNode<T> root) {
            this.root = root;
        }

        private int rank() {
            return root.rank;
        }

        public String toString() {
            return root.toString();
        }

        private void updateSufMin() {
            for (Tree<T> t = this; t != null; t = t.prev) {
                if (t.next == null || t.root.cKey <= t.next.sufMin.root.cKey) {
                    t.sufMin = t;
                } else {
                    t.sufMin = t.next.sufMin;
                }
            }
        }

    }

    private static final class TreeNode<T> {
        private TreeNode<T> left;
        private TreeNode<T> right;
        private boolean leaf;

        private Entry<T> first;
        private Entry<T> last;
        private int nbEntries;
        private double cKey;

        private final int rank;
        private final int size;

        /**
         * Creates a new leaf containing one single, given element.
         * 
         * @param elt
         * @param key
         */
        private TreeNode(final Entry<T> elt, final double key) {
            rank = 0;
            size = 1;

            first = elt;
            last = elt;
            cKey = key;
            nbEntries = 1;

            left = null;
            right = null;
            leaf = true;
        }

        /**
         * Creates a new node combining the two given nodes and preserving the
         * soft heap structure.
         * 
         * @param tree1
         * @param tree2
         */
        private TreeNode(final TreeNode<T> tree1, final TreeNode<T> tree2) {
            assert tree1.rank == tree2.rank;

            rank = tree1.rank + 1;
            size = size(rank);

            first = null;
            last = null;
            cKey = -1;
            nbEntries = 0;

            left = tree1;
            right = tree2;
            leaf = false;
            sift();
        }

        private void swap() {
            final TreeNode<T> temp = right;
            right = left;
            left = temp;
        }

        private Entry<T> pick() {
            final Entry<T> elt = first;
            first = first.next;
            nbEntries--;
            if (!leaf && nbEntries <= size / 2) {
                sift();
            }

            return elt;
        }

        private void sift() {
            while (nbEntries < size && !leaf) {
                if (left == null || (right != null && left.cKey > right.cKey)) {
                    swap();
                }

                concatenate(left);

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

        private void clear() {
            first = null;
            last = null;
            nbEntries = 0;
        }

        private void concatenate(final TreeNode<T> e) {
            if (first == null) {
                first = e.first;
            } else {
                last.next = e.first;
            }
            last = e.last;
            nbEntries += e.nbEntries;
            cKey = e.cKey;
            e.clear();

        }

        public String toString() {
            final StringBuilder stb = new StringBuilder();
            treeString(stb, 0);
            return stb.toString();
        }

        public void treeString(final StringBuilder stb, final int level) {

            for (int i = level; --i >= 0;) {
                stb.append("-");
            }
            stb.append(cKey).append(": {");
            for (Entry<T> e = first; e != null; e = e.next) {
                stb.append(e).append(", ");
            }
            stb.append("}\n");
            if (leaf) {
                return;
            }
            if (left == null) {
                for (int i = level + 1; --i >= 0;) {
                    stb.append("-");
                }
                stb.append("*\n");
            } else {
                left.treeString(stb, level + 1);
            }
            if (right == null) {
                for (int i = level + 1; --i >= 0;) {
                    stb.append("-");
                }
                stb.append("*\n");
            } else {
                right.treeString(stb, level + 1);
            }
        }

    }

    private static final class Entry<T> {
        private final T data;
        private Entry<T> next;
        private int inQueue = -1;

        public Entry(final T data) {
            this.data = data;
        }

        public String toString() {
            return data.toString();
        }
    }
}
