package cspfj.priorityqueues
import java.util.AbstractQueue
import scala.collection.mutable.PriorityQueue
import scala.collection.JavaConversions

final class ScalaNative[T <: PTag](
  val key: Key[T]) extends AbstractQueue[T] {
  
  val queue = new PriorityQueue[T]()(key.reverse)

  def offer(elt: T) = {
    if (elt.isPresent) {
      false
    } else {
      queue.enqueue(elt)
      elt.setPresent()
      true
    }
  }

  def poll() = {
    val elt = queue.dequeue()
    elt.unsetPresent
    elt
  }

  def peek = queue.head

  def size = queue.size

  override def clear() {
    queue.clear()
    PTag.clear()
  }

  def iterator = JavaConversions.asJavaIterator(queue.iterator)
}