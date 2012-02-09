package cspfj.priorityqueues;

import java.util.AbstractQueue
import scala.collection.immutable.Queue
import java.util.Iterator
import scala.annotation.tailrec
import java.util.Arrays
import scala.collection.JavaConversions
import java.io.File
import java.io.PrintStream

//object Stats {
//  val out = new PrintStream(new File("/tmp/stats.csv"))
//}

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

  def this(k: Key[T]) = this(k, 32)

  var first = nbLists

  private def chooseList(element: T): Int = {
    val k = key.getKey(element) - 1
    assert(k >= 0, element.toString + " -> " + k)
    nbLists - Integer.numberOfLeadingZeros(k)
  }

  def offer(e: T) =
    if (e.isPresent) false
    else {
      val list = chooseList(e)
      if (list < first) first = list
      //Stats.out.println(key.getKey(e) + " : " + list)
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
      first = i
      e
    }

  def poll() = {
    val e = poll(first)
    e.unsetPresent()
    e
  }

  override def clear() {
    first = nbLists
    PTag.clear()
    queues.indices.foreach(queues(_) = Queue.empty)
  }

  def iterator = JavaConversions.asJavaIterator(queues.iterator.flatMap(_.iterator))

  def size = queues.map(_.size).sum

  override def isEmpty = {
    @tailrec
    def empty(i: Int): Boolean =
      if (i >= nbLists) true
      else if (queues(i).isEmpty) empty(i + 1)
      else false

    empty(first)
  }

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
