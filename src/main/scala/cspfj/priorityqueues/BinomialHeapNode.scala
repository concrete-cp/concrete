package cspfj.priorityqueues

trait BinomialHeapNode[T <: BinomialHeapNode[T]] extends PTag with LazyKey with DLLNode[T] {
  self: T =>

  var child: Option[T] = None

  var parent: Option[T] = None

  var rank = 0

  override def clearNode() {
    super[DLLNode].clearNode()
    child = None
    parent = None
    rank = 0
  }

  def addSubTree(subTree: T) {
    if (child.isDefined) {
      child.get.add(subTree)
    }
    rank += 1
    child = Some(subTree)
    subTree.parent = Some(this)
  }

  def tree(depth: Int, last: T): String =
    (0 until depth).fold("")((i, acc) => acc + "--") + toString + " (" + key + ", " + rank + ")\n" +
      (if (child.isDefined) child.get.tree(depth + 1, child.get)) +
      (if (right != last) right.tree(depth, last))

}