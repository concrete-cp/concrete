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
final class Fifos[T <: PTag] extends PriorityQueue[T] {

  val NB_LISTS = 8

  val queues: Array[Queue[T]] = Array.fill(NB_LISTS)(Queue.empty)

  var first = NB_LISTS

  var last = -1

  private def chooseList(eval: Int): Int = {
    math.min(NB_LISTS - 1, NB_LISTS - Integer.numberOfLeadingZeros(eval) / 4)
  }

  def offer(e: T, eval: Int) =
    if (e.isPresent) false
    else {
      val list = chooseList(eval)
      //print(list + " ")
      if (list < first) first = list
      if (list > last) last = list
      //Stats.out.println(key.getKey(e) + " : " + list)
      try
        queues(list) = queues(list).enqueue(e)
      catch {
        case exc: IndexOutOfBoundsException =>
          throw new IllegalArgumentException(e + " : " + eval + " -> " + list, exc)
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

  def clear() {
    (first to last).foreach(queues(_) = Queue.empty)
    first = NB_LISTS
    last = -1
    PTag.clear()
  }

  def isEmpty = first > last

}
