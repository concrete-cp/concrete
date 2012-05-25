package cspfj.priorityqueues;

import java.util.AbstractQueue
import scala.collection.immutable.Queue
import java.util.Iterator
import scala.annotation.tailrec
import java.util.Arrays
import scala.collection.JavaConversions
import java.io.File
import java.io.PrintStream
import cspfj.constraint.Constraint

/**
 *
 * @author scand1sk
 *
 * @param <T>
 */
final class GenFifos(key: Key[Constraint]) extends AbstractQueue[Constraint] {

  private class HeadDLNode extends DLNode[Constraint] {
    override def isEmpty = next eq this
    override def clear() {
      next = this
      prev = this
    }
  }

  val NB_LISTS = 8
  
  val FACTOR = math.ceil(31.0 / NB_LISTS).toInt

  val queues: Array[DLNode[Constraint]] = Array.fill(NB_LISTS)(new HeadDLNode())

  var last = -1

  private def chooseList(element: Constraint): Int = {
    val k = key.getKey(element)
    math.min(NB_LISTS - 1, NB_LISTS - Integer.numberOfLeadingZeros(k) / FACTOR)
  }

  def offer(e: Constraint) = {
    val list = chooseList(e)

    if (e.isPresent && list == e.currentList) false
    else {
      if (e.isPresent) {
        e.remove()
        if (queues(e.currentList).isEmpty && last == e.currentList) last -= 1
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
  private def poll(i: Int): Constraint =
    if (i > last) throw new NoSuchElementException
    else if (queues(i).isEmpty) poll(i + 1)
    else {
      val q = queues(i)
      val e = q.next
      e.remove()
      if (i == last && q.isEmpty) last = -1
      e.asInstanceOf[Constraint]
    }

  def poll() = {
    val e = poll(0)
    e.unsetPresent()
    e
  }

  override def clear() {
    (0 to last).foreach(queues(_).clear())
    last = -1
    PTag.clear()
  }

  def iterator = throw new UnsupportedOperationException // JavaConversions.asJavaIterator(queues.iterator.flatMap(_.iterator))

  def size = throw new UnsupportedOperationException // (0 to last).map(i => queues(i).size).sum

  override def isEmpty = last < 0

  @tailrec
  private def peek(i: Int): Constraint = {
    if (i > last) throw new NoSuchElementException
    else if (queues(i).isEmpty) peek(i + 1)
    else queues(i).next.asInstanceOf[Constraint]
  }

  def peek() = peek(0)

}
