package concrete.priorityqueues

final class ScalaNative[T <: PTag] extends PriorityQueue[T] {

  private val EOrdering = Ordering
    .by { e: E => e.eval }
    .reverse

  private case class E(elt: T, eval: Int)

  private val queue = new collection.mutable.PriorityQueue[E]()(EOrdering)

  private val presence = new Presence

  def isEmpty: Boolean = queue.isEmpty

  def offer(elt: T, eval: Int): Boolean = {
    if (presence.isPresent(elt)) {
      false
    } else {
      queue.enqueue(E(elt, eval))
      presence.setPresent(elt)
      true
    }
  }

  def poll(): T = {
    val elt = queue.dequeue().elt
    presence.unsetPresent(elt)
    elt
  }

  override def clear(): Unit = {
    queue.clear()
    presence.clear()
  }
}