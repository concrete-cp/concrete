package concrete.util

object HeapSet {
  def empty(n: Int) = new HeapSet(EmptyHeap, Vector.fill(n)(-1))
}

/**
 * @author vion
 */
class HeapSet(queue: SkewHeap[Int], set: Vector[Int]) {
  def offer(e: Int, eval: Int): HeapSet = new HeapSet(queue.offer(e, eval), set.updated(e, eval))

  def poll: Option[(Int, Int, HeapSet)] = {
    if (queue.isEmpty) {
      None
    } else {
      val e = queue.head
      val eval = queue.eval

      if (set(e) == eval) {
        Some((e, eval, new HeapSet(queue.tail, set)))
      } else {
        new HeapSet(queue.tail, set).poll
      }
    }
  }

  def remove(e: Int, eval: Int): HeapSet = new HeapSet(queue, set.updated(e, -1))
}