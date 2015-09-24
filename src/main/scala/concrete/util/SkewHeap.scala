package concrete.util

/**
 * @author vion
 */
sealed trait SkewHeap[+A] {
  def offer[B >: A](element: B, eval: Int): SkewHeap[B] =
    merge(new SkewHeapNode(element, eval, EmptyHeap, EmptyHeap))
  def head: A
  def eval: Int
  def tail: SkewHeap[A]
  def isEmpty: Boolean = this eq EmptyHeap

  def merge[B >: A](h2: SkewHeap[B]): SkewHeap[B]
}

object EmptyHeap extends SkewHeap[Nothing] {
  def head: Nothing = throw new NoSuchElementException
  def eval: Int = throw new NoSuchElementException
  def tail: SkewHeap[Nothing] = throw new NoSuchElementException
  def merge[B >: Nothing](h2: SkewHeap[B]): SkewHeap[B] = h2
}

final class SkewHeapNode[+A](
    val head: A,
    val eval: Int,
    val left: SkewHeap[A],
    val right: SkewHeap[A]) extends SkewHeap[A] {
  def tail: SkewHeap[A] = left.merge(right)
  def merge[B >: A](h2: SkewHeap[B]): SkewHeap[B] = h2 match {
    case EmptyHeap => this
    case h2: SkewHeapNode[B] =>
      if (eval > h2.eval) {
        new SkewHeapNode(head, eval, h2.merge(right), left)
      } else {
        new SkewHeapNode(h2.head, h2.eval, merge(h2.right), h2.left)
      }
  }
}