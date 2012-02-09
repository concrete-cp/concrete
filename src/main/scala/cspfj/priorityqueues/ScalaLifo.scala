package cspfj.priorityqueues
import java.util.AbstractQueue
import scala.collection.immutable.Queue
import scala.collection.JavaConversions

class ScalaLifo[T <: PTag] extends AbstractQueue[T] {

  var stack: List[T] = Nil

  def this(k: Key[T]) = this()

  def offer(e: T) = {
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