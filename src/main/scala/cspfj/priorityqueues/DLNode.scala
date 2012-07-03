package cspfj.priorityqueues

trait DLNode[This <: DLNode[This]] {
  var currentList = -1
  var prev: DLNode[This] = this
  var next: DLNode[This] = this

  def remove() {
    prev.next = next
    next.prev = prev
  }

  def append(n: This) {
    n.prev = prev
    n.next = this

    prev.next = n
    prev = n
  }

  def isEmpty: Boolean = throw new UnsupportedOperationException

  def clear(): Unit = throw new UnsupportedOperationException
}

final class HeadDLNode[T <: DLNode[T]] extends DLNode[T] {
  override def isEmpty = next eq this
  override def clear() {
    next = this
    prev = this
  }
  def iterator = new Iterator[T] {
    var c = HeadDLNode.this.next
    def hasNext = !(c eq HeadDLNode.this)
    def next() = {
      val r = c
      c = c.next
      r.asInstanceOf[T]
    }
  }
  override def toString() = iterator.mkString("[", ", ", "]")
}