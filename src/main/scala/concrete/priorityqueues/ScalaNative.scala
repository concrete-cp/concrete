package concrete.priorityqueues
import java.util.AbstractQueue
import scala.collection.JavaConversions

final class ScalaNative[T <: PTag] extends PriorityQueue[T] {

  implicit private val EOrdering = Ordering.by { e: E => e.eval }

  private final case class E(elt: T, eval: Int)

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