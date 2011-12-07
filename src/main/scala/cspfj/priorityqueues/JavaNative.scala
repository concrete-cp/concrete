package cspfj.priorityqueues
import java.util.Comparator
import java.util.PriorityQueue
import java.util.AbstractQueue

class JavaNative[T <: PTag](val key: Key[T]) extends AbstractQueue[T] {
  val queue = new PriorityQueue[T](10, new Comparator[T] {
    def compare(x: T, y: T) = key.compare(x, y)
  })

  def offer(elt: T) = {
    if (elt.isPresent) {
      false
    } else {
      queue.offer(elt)
      elt.setPresent()
      true
    }
  }

  def poll() = {
    val elt = queue.poll()
    elt.unsetPresent
    elt
  }

  def peek = queue.peek

  def size = queue.size

  override def clear() {
    queue.clear()
    PTag.clear()
  }

  def iterator = queue.iterator

}