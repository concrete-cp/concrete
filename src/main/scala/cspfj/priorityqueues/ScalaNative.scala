package cspfj.priorityqueues
import java.util.AbstractQueue
import scala.collection.JavaConversions

final class ScalaNative[T <: PTag] extends PriorityQueue[T] {

  private final case class E(elt: T, eval: Int) extends Ordered[E] {
    def compare(y: E) = -eval.compare(y.eval)
  }

  private val queue = new collection.mutable.PriorityQueue[E]()

  def isEmpty = queue.isEmpty
  
  def offer(elt: T, eval: Int) = {
    if (elt.isPresent) {
      false
    } else {
      queue.enqueue(E(elt, eval))
      elt.setPresent()
      true
    }
  }

  def poll() = {
    val elt = queue.dequeue().elt
    elt.unsetPresent
    elt
  }

  override def clear() {
    queue.clear()
    PTag.clear()
  }
}