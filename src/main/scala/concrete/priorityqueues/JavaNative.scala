package concrete.priorityqueues

final class JavaNative[T <: PTag] extends PriorityQueue[T] {

  case class E(e: T, eval: Int) extends Ordered[E] {
    def compare(y: E): Int = eval.compare(y.eval)
  }

  private val queue = new java.util.PriorityQueue[E]()

  private val presence = new Presence

  def offer(elt: T, eval: Int): Boolean = {
    if (presence.isPresent(elt)) {
      false
    } else {
      queue.offer(E(elt, eval))
      presence.setPresent(elt)
      true
    }
  }

  def poll(): T = {
    val elt = queue.poll().e
    presence.unsetPresent(elt)
    elt
  }

  def isEmpty: Boolean = queue.isEmpty

  override def clear(): Unit = {
    queue.clear()
    presence.clear()
  }

}