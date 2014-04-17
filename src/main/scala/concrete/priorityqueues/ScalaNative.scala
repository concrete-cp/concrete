package concrete.priorityqueues
import java.util.AbstractQueue
import scala.collection.JavaConversions

final class ScalaNative[T <: PTag] extends PriorityQueue[T] {

  private final case class E(elt: T, eval: Int) extends Ordered[E] {
    def compare(y: E) = -eval.compare(y.eval)
  }

  private val queue = new collection.mutable.PriorityQueue[E]()
  
  private val presence = new Presence

  def isEmpty = queue.isEmpty
  
  def offer(elt: T, eval: Int) = {
    if (presence.isPresent(elt)) {
      false
    } else {
      queue.enqueue(E(elt, eval))
      presence.setPresent(elt)
      true
    }
  }

  def poll() = {
    val elt = queue.dequeue().elt
    presence.unsetPresent(elt)
    elt
  }

  override def clear() {
    queue.clear()
    presence.clear()
  }
}