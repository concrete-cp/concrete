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
final class SimpleFifos(key: Key[_]) extends AbstractQueue[Constraint] {

  val nbLists = 8

  val queues: Array[Queue[Constraint]] = Array[Queue[Constraint]]().padTo(nbLists, Queue.empty)

  var first = nbLists

  var last = -1

  def offer(e: Constraint) =
    if (e.isPresent) false
    else {
      val list = e.simpleEvaluation
      if (list < first) first = list
      if (list > last) last = list
      //Stats.out.println(key.getKey(e) + " : " + list)
      try
        queues(list) = queues(list).enqueue(e)
      catch {
        case exc: IndexOutOfBoundsException =>
          throw new IllegalArgumentException(e + " : " + e.simpleEvaluation + " -> " + list, exc)
      }
      e.setPresent()
      true
    }

  @tailrec
  private def poll(i: Int): Constraint =
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
    first = nbLists
    last = -1
    PTag.clear()
  }

  def iterator = JavaConversions.asJavaIterator(queues.iterator.flatMap(_.iterator))

  def size = (first to last).map(i => queues(i).size).sum

  override def isEmpty = first > last

  @tailrec
  private def peek(i: Int): Constraint = {
    if (i > last) {
      throw new NoSuchElementException
    } else if (queues(i).isEmpty) {
      peek(i + 1)
    } else {
      queues(i).head
    }
  }

  def peek() = peek(first)

}
