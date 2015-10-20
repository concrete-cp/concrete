package concrete
package cluster

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
    if (visited(root)) {
      Set.empty
    } else {
      root.vars.flatMap(neighbours).foldLeft(visited + root) {
        (visit, n) => visit ++ crawl(n, neighbours, visit)
      }

    }
  }
}

