package cspfj.priorityqueues

trait DLLNode[T <: DLLNode[T]] { self: T =>

  var right = this

  var left = this

  /**
   * Remove this node from the list it appears in.
   */
  def remove() {
    left.right = right;
    right.left = left;
  }

  /**
   * Adds x to the left of this node in the list.
   *
   * @param x
   */
  def add(x: T) {
    x.right = this;
    x.left = left;
    left = x;
    x.left.right = x;
  }

}