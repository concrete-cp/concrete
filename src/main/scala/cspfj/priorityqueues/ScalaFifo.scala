package cspfj.priorityqueues
import java.util.AbstractQueue
import scala.collection.mutable.Queue
import scala.collection.JavaConversions

class ScalaFifo[T <: PTag] extends AbstractQueue[T] {

  val queue = new Queue[T]()

  def this(k: Key[T]) = this()

  def offer(e: T) = {
    if (e.isPresent) {
      false
    } else {
      queue.enqueue(e)
      e.setPresent()
      true
    }
  }

  def poll() = {
    val p = queue.dequeue()
    p.unsetPresent()
    p
  }

  def peek = queue.head

  def size = queue.size

  override def isEmpty = queue.isEmpty

  def iterator = JavaConversions.asJavaIterator(queue.iterator)

  override def clear() {
    PTag.clear()
    queue.clear()
  }

}