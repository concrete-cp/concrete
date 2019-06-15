package concrete.priorityqueues

import scala.annotation.tailrec

final class SimpleFifos[T <: PTag with DLNode[T]] extends PriorityQueue[T] {

  val NB_LISTS = 8

  val queues: Array[DLNode[T]] = Array.fill(NB_LISTS)(new HeadDLNode())

  var last: Int = -1

  private val presence = new Presence

  def offer(e: T, list: Int): Boolean = {
    val present = presence.isPresent(e)
    if (present && list == e.currentList) false
    else {
      if (present) {
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

      presence.setPresent(e)
      true
    }
  }

  @tailrec
  private def poll(i: Int): T = {
    assume(i <= last, s"$$i > $last\n${queues.map(n => n.isEmpty).mkString("\n")}")
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

  def poll(): T = {
    val e = poll(0)
    presence.unsetPresent(e)
    e
  }

  def clear(): Unit = {
    (0 to last).foreach(queues(_).clear())
    last = -1
    presence.clear()
  }

  def isEmpty: Boolean = last < 0

}
