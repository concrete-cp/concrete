package cspfj.priorityqueues;

import java.util.AbstractQueue

import scala.annotation.tailrec

import cspfj.constraint.Constraint

import cspfj.Statistic

/**
 *
 * @author scand1sk
 *
 * @param <T>
 */
final class QuickFifos[T <: PTag with DLNode[T]] extends PriorityQueue[T] {

  @Statistic
  var nbOffer = 0
  @Statistic
  var nbUpdate = 0
  @Statistic
  var nbPoll = 0
  @Statistic
  var nbClear = 0
  @Statistic
  var offerSize = 0L
  @Statistic
  var updateSize = 0L
  @Statistic
  var pollSize = 0L

  private val NB_LISTS = 8

  private val FACTOR = {
    val s = 31.0 / (NB_LISTS)
    (1 until NB_LISTS).map(i => math.pow(2, i * s).toInt).toArray
  }

  private val queues: Array[DLNode[T]] = Array.fill(NB_LISTS)(new HeadDLNode())

  private var last = -1

  var size = 0

  private def chooseList(e: Int): Int = {
    var i = 0
    while (i < NB_LISTS - 1 && e > FACTOR(i)) i += 1
    i
  }

  def offer(e: T, eval: Int) = {
    require(eval >= 0)
    val list = chooseList(eval)
    //println(e + " : " + eval + " -> " + list)

    if (e.isPresent && list == e.currentList) false
    else {
      if (e.isPresent) {
        e.remove()
        if (last == e.currentList) {
          while (last >= 0 && queues(last).isEmpty) last -= 1
        }
        nbUpdate += 1
        updateSize += size
      } else {
        size += 1
        nbOffer += 1
        offerSize += size
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
    require(i <= last, i + " > " + last + "\n" + queues.map(n => n.isEmpty).mkString("\n"))
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
    nbPoll += 1
    pollSize += size
    size -= 1
    val e = poll(0)
    e.unsetPresent()
    e
  }

  def clear() {
    (0 to last).foreach(queues(_).clear())
    last = -1
    size = 0
    nbClear += 1
    PTag.clear()
  }

  def isEmpty = last < 0

  override def toString = queues.mkString("\n")

}
