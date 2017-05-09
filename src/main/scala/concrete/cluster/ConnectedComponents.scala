package concrete
package cluster

import scala.collection.mutable.Queue

object ConnectedComponents extends Cluster {
  def apply[T <: Arc](arcs: Seq[T]): Seq[Seq[T]] = {
    val edges = nodes(arcs)

    var visited: Set[T] = Set.empty
    var components: Seq[Set[T]] = Seq.empty

    for (v <- arcs) {
      val component = crawl[T](v, edges, visited)
      if (component.nonEmpty) {
        components +:= component
        visited ++= component
      }
    }

    components.map(_.toSeq)
  }

  def crawl[A <: Arc](
    root: A,
    neighbours: Map[Variable, Seq[A]],
    visited: Set[A]): Set[A] = {

    val queue = new Queue[A]()
    queue.enqueue(root)
    var component = Set[A]()

    while (queue.nonEmpty) {
      val elem = queue.dequeue()

      if (!visited(elem) && !component(elem)) {
        component += elem

        queue ++= elem.vars.flatMap(neighbours)
      }
    }

    component
  }
}

