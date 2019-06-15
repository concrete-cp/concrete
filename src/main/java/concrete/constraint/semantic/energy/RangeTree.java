package concrete.constraint.semantic.energy;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

@SuppressWarnings("ALL")
public class RangeTree {
    private class Element {
        public int x;
        public int y;
        public int h;

        public Element(int x, int y, int h) {
            this.x = x;
            this.y = y;
            this.h = h;
        }
    }

    private class Pointer {
        public Node node;
        public int left;
        public int right;
        public int y;

        public Pointer(Node node) {
            this.node = node;
            this.left = -1;
            this.right = -1;
            this.y = node.value.y;
        }
    }

    private class Node {
        public Element value;
        public Node parent;
        public Node left;
        public Node right;
        public Pointer[] ys;
        public int slope;
        public int partial;
        public int x;
        public int maxX;
        public int[] partial_sum;
        public int[] slopes;

        public Node(Element value, Node parent) {
            this.value = value;
            this.parent = parent;
            this.left = null;
            this.right = null;
            this.ys = null;
            this.partial_sum = null;
            this.slopes = null;
        }

        public String getText() {
            return String.format("(%d, %d)", value.x, value.y);
        }

        public boolean isLeaf() {
            return right == null && left == null;
        }

        @Override
        public String toString() {
            return Integer.toString(value.x);
        }
    }
    private Point[] points;
    private Node root;

    public RangeTree(Point[] points) {
        this.points = points;
        int n = this.points.length;

        Element[] elements = new Element[n * 2];
        for (int i = 0 ; i < n; i++) {
            elements[i] = new Element(points[i].x1(), points[i].y(), points[i].h());
            elements[n+i] = new Element(points[i].x2(), points[i].y(), -points[i].h());
        }

        Arrays.sort(elements, (e1, e2) -> Integer.compare(e1.x, e2.x));
        root = this.buildTree(elements, null, 0, elements.length);
        buildAssociated(root);
    }

    public int query(int x1, int x2, int y) {
        int pointer = findLessThan(root.ys, y);
        if (pointer == -1)
            return 0;
        int value = root.partial_sum[pointer] - improvedRangeQuery(x1, y, pointer);

        //BruteRangeTree bruteRangeTree = new BruteRangeTree(this.points);
        //assert bruteRangeTree.query(x1, x2, y) == value;

        return value;
    }

    private Node buildTree(Element[] array, Node parent, int l, int r) {
        int lenght = r - l;
        if (lenght <= 0)
            return null;
        if (lenght == 1)
            return new Node(array[l], parent);

        int center = l + divide_round_up(lenght, 2) - 1;
        Node node = new Node(array[center], parent);
        node.left = buildTree(array, node, l, center + 1);
        node.right = buildTree(array, node, center+1, r);

        return node;
    }

    private Pointer[] buildAssociated(Node node) {
        if (node == null) {
            return new Pointer[0];
        }
        if (node.isLeaf()) {
            node.ys = new Pointer[]{new Pointer(node)};
            node.partial_sum = new int[]{0};
            node.slopes = new int[]{node.value.h};
            node.slope = node.value.h;
            node.x = node.value.x;
            node.maxX = node.x;
            return node.ys;
        }

        Pointer[] left = buildAssociated(node.left);
        Pointer[] right = buildAssociated(node.right);

        int n = left.length + right.length;
        Pointer[] ys = new Pointer[n];
        int[] partialSum = new int[n];
        int[] slopes = new int[n];
        int i = 0, j = 0, k = 0;

        node.maxX = Math.max(node.left.maxX, node.right.maxX);

        while (k < n) {
            if (i < left.length && (j >= right.length || left[i].y < right[j].y)) {
                Pointer current = left[i];
                Pointer p = new Pointer(current.node);
                p.left = i;
                if (j == 0) {
                    p.right = - 1;
                } else {
                    p.right = j - 1;
                }

                // Just to remind you, future Yan, the node pointed by a Pointer object is always a leaf.
                // Thus, it has always vectors of size one.
                ys[k] = p;
                if (k > 0) {
                    slopes[k] = slopes[k-1] + current.node.slopes[0];
                    partialSum[k] = partialSum[k-1] + current.node.slopes[0] * (node.maxX - current.node.x);
                } else {
                    slopes[k] = current.node.slopes[0];
                    partialSum[k] = current.node.slopes[0] * (node.maxX - current.node.x);
                }
                i += 1;
            } else {
                Pointer current = right[j];
                Pointer p = new Pointer(current.node);
                p.right = j;
                if (i == 0) {
                    p.left = - 1;
                } else {
                    p.left= i - 1;
                }

                ys[k] = p;
                if (k > 0) {
                    slopes[k] = slopes[k-1] + current.node.slopes[0];
                    partialSum[k] = partialSum[k-1] + current.node.slopes[0] * (node.maxX - current.node.x);
                } else {
                    slopes[k] = current.node.slopes[0];
                    partialSum[k] = current.node.slopes[0] * (node.maxX - current.node.x);
                }
                j += 1;
            }
            k += 1;
        }

        node.ys = ys;
        node.slopes = slopes;
        node.partial_sum = partialSum;

        node.slope = node.left.slope + node.right.slope;
        int x = Math.max(node.left.value.x, node.left.value.x);
        node.x = x;

        return node.ys;
    }

    private void appendArray(int[] array, int i, int value) {
        int sum = 0;
        if (i > 0)
            sum = array[i-1];
        array[i] = sum + value;
    }

    private int improvedReport(Node v, int x, int index) {
        if (v == null)
            return 0;
        return v.partial_sum[index] + v.slopes[index] * (x - v.maxX);
    }

    private int improvedRangeQuery(int x, int y, int pointer) {
        Node vSplit = root;
        int sum = 0;

        Node v = vSplit;
        while (!v.isLeaf()) {
            if (pointer == -1)
                break;
            if (x >= v.value.x) {
                int index = v.ys[pointer].left;
                if (index >= 0)
                    sum += improvedReport(v.left, x, index);
                pointer = v.ys[pointer].right;
                v = v.right;
            } else {
                pointer = v.ys[pointer].left;
                v = v.left;
            }
        }

        if (x >= v.value.x && pointer >= 0) {
            sum += improvedReport(v, x, pointer);
        }

        return Math.max(0, sum);
    }

    private int findLessThan(Pointer[] array, int value) {
        int min = -1;
        for (int i = 0; i < array.length; i++) {
            if (array[i].y <= value) {
                min = i;
            } else {
                return min;
            }
        }
        return min;
    }

    private Node findSplitNode(int x1, int x2) {
        Node v = root;
        while (!v.isLeaf() && (x2 <= v.value.x || x1 > v.value.x)) {
            if (x2 < v.value.x)
                v = v.left;
            else
                v = v.right;
        }
        if (v.isLeaf())
            v = v.parent;
        return v;
    }

    private int divide_round_up(int a, int b) {
        return a % b == 0 ? a / b : a / b + 1;
    }

    //http://stackoverflow.com/questions/4965335/how-to-print-binary-tree-diagram#answer-29704252
    public static void print(Node root)
    {
        List<List<String>> lines = new ArrayList<List<String>>();

        List<Node> level = new ArrayList<Node>();
        List<Node> next = new ArrayList<Node>();

        level.add(root);
        int nn = 1;

        int widest = 0;

        while (nn != 0) {
            List<String> line = new ArrayList<String>();

            nn = 0;

            for (Node n : level) {
                if (n == null) {
                    line.add(null);

                    next.add(null);
                    next.add(null);
                } else {
                    String aa = n.getText();
                    line.add(aa);
                    if (aa.length() > widest) widest = aa.length();

                    next.add(n.left);
                    next.add(n.right);

                    if (n.left != null) nn++;
                    if (n.right != null) nn++;
                }
            }

            if (widest % 2 == 1) widest++;

            lines.add(line);

            List<Node> tmp = level;
            level = next;
            next = tmp;
            next.clear();
        }

        int perpiece = lines.get(lines.size() - 1).size() * (widest + 4);
        for (int i = 0; i < lines.size(); i++) {
            List<String> line = lines.get(i);
            int hpw = (int) Math.floor(perpiece / 2f) - 1;

            if (i > 0) {
                for (int j = 0; j < line.size(); j++) {

                    // split node
                    char c = ' ';
                    if (j % 2 == 1) {
                        if (line.get(j - 1) != null) {
                            c = (line.get(j) != null) ? '┴' : '┘';
                        } else {
                            if (j < line.size() && line.get(j) != null) c = '└';
                        }
                    }
                    System.out.print(c);

                    // lines and spaces
                    if (line.get(j) == null) {
                        for (int k = 0; k < perpiece - 1; k++) {
                            System.out.print(" ");
                        }
                    } else {

                        for (int k = 0; k < hpw; k++) {
                            System.out.print(j % 2 == 0 ? " " : "─");
                        }
                        System.out.print(j % 2 == 0 ? "┌" : "┐");
                        for (int k = 0; k < hpw; k++) {
                            System.out.print(j % 2 == 0 ? "─" : " ");
                        }
                    }
                }
                System.out.println();
            }

            // print line of numbers
            for (int j = 0; j < line.size(); j++) {

                String f = line.get(j);
                if (f == null) f = "";
                int gap1 = (int) Math.ceil(perpiece / 2f - f.length() / 2f);
                int gap2 = (int) Math.floor(perpiece / 2f - f.length() / 2f);

                // a number
                for (int k = 0; k < gap1; k++) {
                    System.out.print(" ");
                }
                System.out.print(f);
                for (int k = 0; k < gap2; k++) {
                    System.out.print(" ");
                }
            }
            System.out.println();

            perpiece /= 2;
        }
    }
}
