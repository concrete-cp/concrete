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
 * @author scand1sk
 *
 * @param <T>
 */
final class Lifos[T <: PTag](val nbLists: Int) extends PriorityQueue[T] {

  val KEY_FACTOR = 3.0;

  val queues: Array[List[T]] = Array[List[T]]().padTo(nbLists, Nil)

  def this() = this(8)

  var first = nbLists

  var last = -1

  private def chooseList(k: Int): Int = {
    nbLists - Integer.numberOfLeadingZeros(k)
    math.max(nbLists - 1, nbLists - Integer.numberOfLeadingZeros(k))
  }

  def offer(e: T, eval: Int) =
    if (e.isPresent) false
    else {
      val list = chooseList(eval)
      if (list < first) first = list
      if (list > last) last = list
      //Stats.out.println(key.getKey(e) + " : " + list)
      try queues(list) ::= e
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
      val e = queues(i)
      queues(i) = e.tail
      first = i
      if (i == last && e.tail.isEmpty) last = -1
      e.head
    }

  def poll() = {
    val e = poll(first)
    e.unsetPresent()
    e
  }

  def clear() {
    (first to last).foreach(queues(_) = Nil)
    first = nbLists
    last = -1
    PTag.clear()
  }

  def isEmpty = first > last

}
