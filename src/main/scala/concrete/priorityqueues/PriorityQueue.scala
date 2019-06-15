package concrete.priorityqueues

trait PriorityQueue[T] {
    def offer(e: T, eval: Int): Boolean
    def poll(): T
    def clear(): Unit
    def isEmpty: Boolean
}