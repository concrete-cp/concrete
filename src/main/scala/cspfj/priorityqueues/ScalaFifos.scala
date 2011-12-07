package cspfj.priorityqueues;

import java.util.AbstractQueue
import scala.collection.mutable.Queue
import java.util.Iterator;
import scala.annotation.tailrec

/**
 * Very simple FIFO queue based on LinkedList. All Identified elements are
 * unique.
 *
 * @author scand1sk
 *
 * @param <T>
 */
final class ScalaFifos[T <: PTag](val key: Key[T], val nbLists: Int) extends AbstractQueue[T] {

  val KEY_FACTOR = 3.0;

  val queues: IndexedSeq[Queue[T]] = (0 until nbLists) map (_ => new Queue[T]())

  def this(k: Key[T]) = this(k, 8)

  @tailrec
  private def chooseList(k: Double, treshold: Double, i: Int): Int = {
    if (i >= nbLists - 1 || k < treshold) {
      i
    } else {
      chooseList(k, treshold * KEY_FACTOR, i + 1)
    }
  }

  private def chooseList(element: T): Int = chooseList(key.getKey(element), KEY_FACTOR, 0)

  def offer(e: T) = {
    if (e.isPresent) {
      false
    } else {
      queues(chooseList(e)).enqueue(e)
      e.setPresent()
      true
    }
  }

  @tailrec
  private def poll(i: Int): T = {
    if (i >= nbLists) {
      throw new NoSuchElementException
    } else if (queues(i).isEmpty) {
      poll(i + 1)
    } else {
      queues(i).dequeue()
    }
  }

  def poll() = {
    val e = poll(0)
    e.unsetPresent()
    e
  }

  override def clear() {
    PTag.clear()
    queues.foreach(_.clear())
  }

  def iterator =
    throw new UnsupportedOperationException();

  def size = queues.map(_.size).sum

  override def isEmpty = queues.forall(_.isEmpty)

  @tailrec
  private def peek(i: Int): T = {
    if (i >= nbLists) {
      throw new NoSuchElementException
    } else if (queues(i).isEmpty) {
      peek(i + 1)
    } else {
      queues(i).head
    }
  }

  def peek() = peek(0)

}
