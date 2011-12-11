package cspfj.priorityqueues

trait DLLNode[T <: DLLNode[T]] { self: T =>

  var right = this
  var left = this

  def clearNode() {
    right = this
    left = this
  }

  /**
   * Remove this node from the list it appears in.
   */
  def remove() {
    left.right = right;
    right.left = left;
    //clearNode()
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

  def merge(x: T) {
    val minLeft = this.left;
    val xLeft = x.left;
    this.left = xLeft;
    xLeft.right = this
    x.left = minLeft;
    minLeft.right = x;
  }

}