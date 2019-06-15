package concrete.priorityqueues

trait DLNode[A <: DLNode[A]] {
  var currentList = -1
  var prev: DLNode[A] = this
  var next: DLNode[A] = this

  def remove() : Unit = {
    prev.next = next
    next.prev = prev
  }

  def append(n: A): Unit = {
    n.prev = prev
    n.next = this

    prev.next = n
    prev = n
  }

  def isEmpty: Boolean = throw new UnsupportedOperationException

  def clear(): Unit = throw new UnsupportedOperationException

  def iterator: Iterator[A] = throw new UnsupportedOperationException

}

final class HeadDLNode[T <: DLNode[T]] extends DLNode[T] {
  override def isEmpty: Boolean = next eq this
  override def clear(): Unit = {
    next = this
    prev = this
  }
  override def iterator: Iterator[T] = new Iterator[T] {
    var c: DLNode[T] = HeadDLNode.this.next
    def hasNext: Boolean = !(c eq HeadDLNode.this)
    def next(): T = {
      val r = c
      c = c.next
      r.asInstanceOf[T]
    }
  }
  override def toString: String = iterator.mkString("[", ", ", "]")
}
