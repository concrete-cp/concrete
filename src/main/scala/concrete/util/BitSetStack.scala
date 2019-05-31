package concrete.util

import org.eclipse.collections.impl.stack.mutable.primitive.IntArrayStack

final class BitSetStack(size: Int) {
  private val inStack = new ScalaBitSet(size)
  private val queue = new IntArrayStack()

  def push(i: Int): Unit = {
    if (inStack.add(i)) {
      queue.push(i)
    }
  }

  def pop(): Int = {
    val value = queue.pop()
    inStack -= value
    value
  }

  def contains(i: Int): Boolean = inStack(i)

  def isEmpty: Boolean = queue.isEmpty

  def nonEmpty: Boolean = queue.notEmpty
}
