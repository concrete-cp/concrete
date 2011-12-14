package cspfj.priorityqueues

trait DLLNode[T <: DLLNode[T]] { self: T =>

  private var _right = this
  private var _left = this

  def right = _right
  def left = _left

  def clearNode() {
    clearDL()
  }

  /**
   * Remove this node from the list it appears in.
   */
  def remove() {
    //val right = this.right
    _left._right = _right;
    _right._left = _left;
//
//    assert(_left.checkLone)
//    assert(_right.checkLone)
    clearDL()
    //    left = this
    //    right = this
  }

  def checkLone = {
    (_left != this || _right == this) &&
      (_right != this || _left == this) &&
      (_left._right == this) && (_right._left == this)

  }

  def clearDL() {
    _right = this
    _left = this
  }

  /**
   * Adds x to the left of this node in the list.
   *
   * @param x
   */
  def add(x: T) {
    x._right = this;
    x._left = _left;
    _left._right = x
    _left = x;

    assert(_left != this)
    assert(_right != this)
    assert(_left._right == this)
    assert(_right._left == this)

    //x.left.right = x;
  }

  def merge(x: T) {
    val minLeft = this.left;
    val xLeft = x.left;

    _left = xLeft;
    xLeft._right = this
    x._left = minLeft;
    minLeft._right = x;
  }

}