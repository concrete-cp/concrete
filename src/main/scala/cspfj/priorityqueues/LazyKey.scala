package cspfj.priorityqueues

trait LazyKey[T <: LazyKey[T]] { //extends Ordered[T] {
  var key = 0.0

  def <(that: T) = key < that.key
}