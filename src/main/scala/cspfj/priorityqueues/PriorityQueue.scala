package cspfj.priorityqueues

trait PriorityQueue[T] {
    def offer(e: T, eval: Int): Boolean
    def poll(): T
    def clear()
    def isEmpty(): Boolean
}