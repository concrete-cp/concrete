package concrete.priorityqueues
import java.util.Comparator

final class JavaNative[T <: PTag] extends PriorityQueue[T] {

  case class E(val e: T, val eval: Int) extends Ordered[E] {
    def compare(y: E) = eval.compare(y.eval)
  }

  private val queue = new java.util.PriorityQueue[E]()

  private val presence = new Presence

  def offer(elt: T, eval: Int) = {
    if (presence.isPresent(elt)) {
      false
    } else {
      queue.offer(E(elt, eval))
      presence.setPresent(elt)
      true
    }
  }

  def poll() = {
    val elt = queue.poll().e
    presence.unsetPresent(elt)
    elt
  }

  def isEmpty = queue.isEmpty

  override def clear() {
    queue.clear()
    presence.clear()
  }

}