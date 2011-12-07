package cspfj.priorityqueues
import java.util.AbstractQueue

class SkewHeap[T <: PTag](
  val key: Key[T]) extends AbstractQueue[T] {

  var root: Node = null

  def offer(elt: T) = {
    if (elt.isPresent) {
      false
    } else {
      root = merge(root, Node(elt, null, null))
      elt.setPresent
      true
    }
  }

  def peek = root.value

  def poll() = {
    val elt = root.value

    if (root.rChild == null) {
      root = merge(root.rChild, root.lChild)
    } else {
      root = merge(root.lChild, root.rChild)
    }

    elt.unsetPresent

    elt
  }

  override def clear() {
    root = null
    PTag.clear()
  }

  override def isEmpty = root == null

  private def size(n: Node): Int = {
    if (n == null) 0
    else 1 + size(n.rChild) + size(n.lChild)
  }

  def size = size(root)

  def merge(n0: Node, n1: Node): Node = {
    if (n0 == null) {
      n1
    } else if (n0 < n1) {
      Node(n0.value, merge(n0.rChild, n1), n0.lChild)
    } else {
      Node(n1.value, merge(n1.rChild, n0), n1.lChild)
    }
  }

  def iterator = throw new UnsupportedOperationException

  case class Node(
    val value: T,
    val lChild: Node,
    val rChild: Node) extends Ordered[Node] {

    val k = key.getKey(value)

    def compare(that: Node) = k.compare(that.k)
  }
}