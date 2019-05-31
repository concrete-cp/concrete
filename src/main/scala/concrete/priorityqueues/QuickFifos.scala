package concrete.priorityqueues;

import scala.annotation.tailrec

import cspom.Statistic

/**
 *
 * @author scand1sk
 *
 * @param <T>
 */
final class QuickFifos[T <: PTag with DLNode[T]] extends PriorityQueue[T] {

  @Statistic
  var nbOffer = 0L
  @Statistic
  var nbUpdate = 0L
  @Statistic
  var nbPoll = 0L
  @Statistic
  var nbClear = 0L

  private val NB_LISTS = 8

  private val FACTOR = Array(10, 100, 1000, 10000, 100000, 1000000, 10000000)

  private val queues: Array[DLNode[T]] = Array.fill(NB_LISTS)(new HeadDLNode())

  private val presence = new Presence

  private var last = -1

  var size = 0

  private def chooseList(e: Int): Int = {
    var i = 0
    while (i < NB_LISTS - 1 && e > FACTOR(i)) i += 1
    i
  }

  def offer(e: T, eval: Int): Boolean = {
    assert(eval >= 0)
    val list = chooseList(eval)
    //println(e + " : " + eval + " -> " + list)

    val present = presence.isPresent(e)

    if (present && list == e.currentList) {
      false
    } else {
      if (present) {
        e.remove()
        if (last == e.currentList) {
          while (last >= 0 && queues(last).isEmpty) last -= 1
        }
        nbUpdate += 1
      } else {
        size += 1
        nbOffer += 1
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
    assume(i <= last, s"$i > $last\n" + queues.map(n => n.isEmpty).mkString("\n"))
    val q = queues(i)
    if (q.isEmpty) {
      poll(i + 1)
    } else {
      val e = q.next
      e.remove()
      if (i == last && q.isEmpty) {
        last = -1
      }
      assert(last >= 0 || queues.forall(_.isEmpty))
      e.asInstanceOf[T]
    }
  }

  def poll(): T = {
    nbPoll += 1
    size -= 1
    val e = poll(0)
    presence.unsetPresent(e)
    e
  }

  def clear() {
    (0 to last).foreach(queues(_).clear())
    last = -1
    size = 0
    nbClear += 1
    presence.clear()
  }

  def isEmpty: Boolean = last < 0

  override def toString: String = queues.mkString("\n")

}
