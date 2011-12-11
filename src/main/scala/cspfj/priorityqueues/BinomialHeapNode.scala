package cspfj.priorityqueues

trait BinomialHeapNode[T >: Null <: BinomialHeapNode[T]] extends PTag with LazyKey[T] with DLLNode[T] {
  self: T =>

  var child: T = null

  var parent: T = null

  var rank = 0

  override def clearNode() {
    super[DLLNode].clearNode()
    child = null
    parent = null
    rank = 0
  }

  def addSubTree(subTree: T) {
    if (child == null) {
      child = subTree
    } else {
      child.add(subTree)
    }
    rank += 1
    subTree.parent = this
  }

  def treeSize: Int = treeSize(this)

  private def treeSize(last: T): Int = {
    1 +
      (if (child != null) child.treeSize else 0) +
      (if (right != last) right.treeSize(last) else 0)
  }

  def tree(depth: Int, last: T): String =
    (0 until depth).map(_ => "--").fold("")(_ + _) + toString + " (" + key + ", " + rank + ")\n" +
      (if (child != null) child.tree(depth + 1, child) else "") +
      (if (right != last) right.tree(depth, last) else "")

}