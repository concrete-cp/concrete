package cspfj.priorityqueues;

import java.util.AbstractQueue
import scala.collection.immutable.Queue
import java.util.Iterator
import scala.annotation.tailrec
import java.util.Arrays
import scala.collection.JavaConversions
import java.io.File
import java.io.PrintStream

/**
 *
 * @author scand1sk
 *
 * @param <T>
 */
final class Fifos[T <: PTag](val key: Key[T]) extends AbstractQueue[T] {

  val NB_LISTS = 8

  val queues: Array[Queue[T]] = Array.fill(NB_LISTS)(Queue.empty)

  var first = NB_LISTS

  var last = -1

  private def chooseList(element: T): Int = {
    val k = key.getKey(element)
    math.min(NB_LISTS - 1, NB_LISTS - Integer.numberOfLeadingZeros(k) / 4)
  }

  def offer(e: T) =
    if (e.isPresent) false
    else {
      val list = chooseList(e)
      //print(list + " ")
      if (list < first) first = list
      if (list > last) last = list
      //Stats.out.println(key.getKey(e) + " : " + list)
      try
        queues(list) = queues(list).enqueue(e)
      catch {
        case exc: IndexOutOfBoundsException =>
          throw new IllegalArgumentException(e + " : " + key.getKey(e) + " -> " + list, exc)
      }
      e.setPresent()
      true
    }

  @tailrec
  private def poll(i: Int): T =
    if (i > last) throw new NoSuchElementException
    else if (queues(i).isEmpty) poll(i + 1)
    else {
      val (e, q) = queues(i).dequeue
      queues(i) = q
      first = i
      if (i == last && q.isEmpty) last = -1
      e
    }

  def poll() = {
    val e = poll(first)
    e.unsetPresent()
    e
  }

  override def clear() {
    (first to last).foreach(queues(_) = Queue.empty)
    first = NB_LISTS
    last = -1
    PTag.clear()
  }

  def iterator = JavaConversions.asJavaIterator(queues.iterator.flatMap(_.iterator))

  def size = (first to last).map(i => queues(i).size).sum

  override def isEmpty = first > last

  @tailrec
  private def peek(i: Int): T = {
    if (i > last) {
      throw new NoSuchElementException
    } else if (queues(i).isEmpty) {
      peek(i + 1)
    } else {
      queues(i).head
    }
  }

  def peek = peek(first)

}
