package cspfj.priorityqueues;

import java.util.AbstractQueue
import scala.collection.immutable.Queue
import java.util.Iterator
import scala.annotation.tailrec
import java.util.Arrays
import scala.collection.JavaConversions

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

  val queues: Array[Queue[T]] = Array[Queue[T]]().padTo(nbLists, Queue.empty)

  def this(k: Key[T]) = this(k, 8)

  @tailrec
  private def chooseList(k: Double, treshold: Double, i: Int): Int =
    if (i >= nbLists - 1 || k < treshold) i
    else chooseList(k, treshold * KEY_FACTOR, i + 1)

  private def chooseList(element: T): Int = chooseList(key.getKey(element), KEY_FACTOR, 0)

  def offer(e: T) =
    if (e.isPresent) false
    else {
      val list = chooseList(e)
      queues(list) = queues(list).enqueue(e)
      e.setPresent()
      true
    }

  @tailrec
  private def poll(i: Int): T =
    if (i >= nbLists) throw new NoSuchElementException
    else if (queues(i).isEmpty) poll(i + 1)
    else {
      val (e, q) = queues(i).dequeue
      queues(i) = q
      e
    }

  def poll() = {
    val e = poll(0)
    e.unsetPresent()
    e
  }

  override def clear() {
    PTag.clear()
    queues.indices.foreach(queues(_) = Queue.empty)
  }

  def iterator = JavaConversions.asJavaIterator(queues.iterator.flatMap(_.iterator))

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
