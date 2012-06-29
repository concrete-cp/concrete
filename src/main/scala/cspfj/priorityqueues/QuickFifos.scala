package cspfj.priorityqueues;

import java.util.AbstractQueue

import scala.annotation.tailrec

import cspfj.constraint.Constraint

trait DLNode[This <: DLNode[This]] {
  var currentList = -1
  var prev: DLNode[This] = this
  var next: DLNode[This] = this

  def remove() {
    prev.next = next
    next.prev = prev
  }

  def append(n: This) {
    n.prev = prev
    n.next = this

    prev.next = n
    prev = n
  }

  def isEmpty: Boolean = throw new UnsupportedOperationException

  def clear(): Unit = throw new UnsupportedOperationException
}

/**
 *
 * @author scand1sk
 *
 * @param <T>
 */
final class QuickFifos extends PriorityQueue[Constraint] {

  private class HeadDLNode extends DLNode[Constraint] {
    override def isEmpty = next eq this
    override def clear() {
      next = this
      prev = this
    }
  }

  val NB_LISTS = 8

  val FACTOR = {
    val s = 31 / NB_LISTS
    var i = 1
    (for (j <- 0 until NB_LISTS) yield {
      i <<= s
      i
    }) toArray
  }

  val queues: Array[DLNode[Constraint]] = Array.fill(NB_LISTS)(new HeadDLNode())

  var last = -1

  private def chooseList(e: Int): Int = {
    var i = 0
    while (i < NB_LISTS && e > FACTOR(i)) i += 1
    i
  }

  def offer(e: Constraint, eval: Int) = {
    require(eval >= 0)
    val list = chooseList(eval)

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
  private def poll(i: Int): Constraint = {
    require(i <= last, i + " > " + last + "\n" + queues.map(n => n.isEmpty).mkString("\n"))
    val q = queues(i)
    if (q.isEmpty) poll(i + 1)
    else {
      val e = q.next
      e.remove()
      if (i == last && q.isEmpty) last = -1
      assert(last >= 0 || queues.forall(_.isEmpty))
      e.asInstanceOf[Constraint]
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
