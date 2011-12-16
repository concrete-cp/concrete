package cspfj.priorityqueues

trait IOBinomialHeapNode[T >: Null <: IOBinomialHeapNode[T]] extends PTag with LazyKey[T] {
  self: T =>

  var child: T = null

  var next: T = null

  var rank = 0

  def clearNode() {
    child = null
    next = null
    rank = 0
  }

  def addSubTree(subTree: T) {
    subTree.next = child
    child = subTree
    rank += 1
  }

  def treeSize: Int = {
    1 +
      (if (child != null) child.treeSize else 0) +
      (if (next != null) next.treeSize else 0)
  }

  def tree(depth: Int): String =
    (0 until depth).map(_ => "--").fold("")(_ + _) + toString + " (" + key + ", " + rank + ")\n" +
      (if (child != null) child.tree(depth + 1) else "") +
      (if (next != null) next.tree(depth) else "")

}