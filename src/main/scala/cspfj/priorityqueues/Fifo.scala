package cspfj.priorityqueues
import java.util.AbstractQueue
import scala.collection.immutable.Queue
import scala.collection.JavaConversions

class Fifo[T <: PTag] extends AbstractQueue[T] {

  var queue: Queue[T] = Queue.empty
 
  def this(k: Key[T]) = this()

  def offer(e: T) = {
    if (e.isPresent) {
      false
    } else {
      queue = queue.enqueue(e)
      e.setPresent()
      true
    }
  }

  def poll() = {
    val (p, q) = queue.dequeue
    queue = q
    p.unsetPresent()
    p
  }

  def peek = queue.head

  def size = queue.size

  override def isEmpty = queue.isEmpty

  def iterator = JavaConversions.asJavaIterator(queue.iterator)

  override def clear() {
    PTag.clear()
    queue = Queue.empty
  }

}