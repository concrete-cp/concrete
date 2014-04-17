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

  val presence = new Presence

  def offer(e: T, eval: Int) = {
    if (presence.isPresent(e)) false
    else {
      queue.append(e)
      presence.setPresent(e)
      true
    }
  }

  def poll() = {
    val e = queue.next
    e.remove()
    val e2 = e.asInstanceOf[T]
    presence.unsetPresent(e2)
    e2
  }

  def clear() {
    queue.clear()
    presence.clear()
  }

  def isEmpty = queue.isEmpty

}
