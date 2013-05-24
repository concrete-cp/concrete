package concrete.priorityqueues;

import java.util.AbstractQueue

import scala.annotation.tailrec

import concrete.constraint.Constraint

/**
 *
 * @author scand1sk
 *
 * @param <T>
 */
final class QuickFifo[T <: PTag with DLNode[T]] extends PriorityQueue[T] {

  val queue = new HeadDLNode[T]()

  def offer(e: T, eval: Int) = {
    if (e.isPresent) false
    else {
      queue.append(e)
      e.setPresent()
      true
    }
  }

  def poll() = {
    val e = queue.next
    e.remove()
    val e2 = e.asInstanceOf[T]
    e2.unsetPresent()
    e2
  }

  def clear() {
    queue.clear()
    PTag.clear()
  }

  def isEmpty = queue.isEmpty

}
