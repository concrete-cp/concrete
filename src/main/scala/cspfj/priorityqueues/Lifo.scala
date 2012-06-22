package cspfj.priorityqueues

import scala.collection.JavaConversions

class Lifo[T <: PTag] extends PriorityQueue[T] {

  var stack: List[T] = Nil

  def offer(e: T, eval: Int) = {
    if (e.isPresent) false
    else {
      stack ::= e
      e.setPresent()
      true
    }
  }

  def poll() = {
    val p = stack.head
    stack = stack.tail
    p.unsetPresent()
    p
  }

  def peek = stack.head

  def size = stack.size

  override def isEmpty = stack == Nil

  def iterator = JavaConversions.asJavaIterator(stack.iterator)

  override def clear() {
    PTag.clear()
    stack = Nil
  }

}