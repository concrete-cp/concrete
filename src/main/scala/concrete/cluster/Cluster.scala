package concrete
package cluster

trait Cluster {
  def apply[A <: Arc](lin: Seq[A]): Seq[Seq[A]]

  def nodes[A <: Arc](arcs: Seq[A]): Map[Variable, Seq[A]] = {
    var edges = Map[Variable, Set[A]]().withDefaultValue(Set.empty)

    for (a <- arcs; v <- a.vars) {
      edges += v -> (edges(v) + a)
    }

    edges.map { case (k, v) => k -> v.toSeq }
  }
}