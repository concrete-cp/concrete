package cspfj.priorityqueues

trait Key[T] extends Ordering[T] {
  def getKey(o: T): Int

  def compare(x: T, y: T) = getKey(x).compareTo(getKey(y))
}

