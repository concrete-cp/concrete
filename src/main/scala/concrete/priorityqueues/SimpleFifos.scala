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
final class SimpleFifos[T <: PTag with DLNode[T]] extends PriorityQueue[T] {

  val NB_LISTS = 8

  val queues: Array[DLNode[T]] = Array.fill(NB_LISTS)(new HeadDLNode())

  var last = -1

  def offer(e: T, list: Int) = {
    if (e.isPresent && list == e.currentList) false
    else {
      if (e.isPresent) {
        e.remove()
        if (last == e.currentList) {
          while (last >= 0 && queues(last).isEmpty) last -= 1
        }
      }
      //print(list + " ")
      if (list > last) last = list
      //Stats.out.println(key.getKey(e) + " : " + list)

      queues(list).append(e)
      e.currentList = list

      e.setPresent()
      true
    }
  }

  @tailrec
  private def poll(i: Int): T = {
    assume(i <= last, i + " > " + last + "\n" + queues.map(n => n.isEmpty).mkString("\n"))
    val q = queues(i)
    if (q.isEmpty) poll(i + 1)
    else {
      val e = q.next
      e.remove()
      if (i == last && q.isEmpty) last = -1
      assert(last >= 0 || queues.forall(_.isEmpty))
      e.asInstanceOf[T]
    }
  }

  def poll() = {
    val e = poll(0)
    e.unsetPresent()
    e
  }

  def clear() {
    (0 to last).foreach(queues(_).clear())
    last = -1
    PTag.clear()
  }

  def isEmpty = last < 0

}
