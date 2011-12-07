package cspfj.priorityqueues

trait Key[T] extends Ordering[T] {
  def getKey(o: T): Double

  def compare(x: T, y: T) = getKey(x).compareTo(getKey(y))
}

