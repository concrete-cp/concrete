package cspfj.priorityqueues
import java.util.AbstractQueue
import scala.collection.immutable.Queue
import scala.collection.JavaConversions

class Fifo[T <: PTag] extends PriorityQueue[T] {

  var queue: Queue[T] = Queue.empty

  def offer(e: T, eval: Int) = {
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

  override def isEmpty = queue.isEmpty

  def iterator = JavaConversions.asJavaIterator(queue.iterator)

  override def clear() {
    PTag.clear()
    queue = Queue.empty
  }

}