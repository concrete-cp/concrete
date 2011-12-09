package cspfj.priorityqueues

trait BinomialHeapNode[T <: BinomialHeapNode[T]] extends PTag with LazyKey {

  var child: BinomialHeapNode[T] = null

  var right: BinomialHeapNode[T] = null

  var parent: BinomialHeapNode[T] = null

  var rank = 0

  def clearNode() {
    child = null
    right = null
    parent = null
    rank = 0
  }

  def addSubTree(subTree: BinomialHeapNode[T]) {
    if (child != null) {
      subTree.right = child
    }
    rank += 1
    child = subTree
    subTree.parent = this
  }

}