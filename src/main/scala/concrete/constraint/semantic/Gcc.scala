package concrete.constraint.semantic;

import java.util.Arrays

import scala.collection.mutable.Queue

import concrete.Contradiction
import concrete.Outcome
import concrete.ProblemState
import concrete.Variable
import concrete.constraint.Constraint
import concrete.Event

final case class Bounds(val value: Int, val minCount: Int, val maxCount: Int) {
  override def toString = value + ": [" + minCount + ", " + maxCount + "]"
}

final class Gcc(scope: Array[Variable], bounds: Array[Bounds]) extends Constraint(scope) {

  def init(ps: ProblemState) = ps

  val offset = bounds.map(_.value).min

  val _bounds2 = new Array[Bounds](bounds.map(_.value).max - offset + 1)
  for (b <- bounds) {
    _bounds2(b.value - offset) = b
  }

  def bound(v: Int) = _bounds2(v - offset)

  val counts = Array.ofDim[Int](1 + scope.map(_.initDomain.last).max)
  val singles = Array.ofDim[Int](1 + scope.map(_.initDomain.last).max)

  def check(t: Array[Int]): Boolean = {

    var counts: Map[Int, Int] = Map.empty.withDefaultValue(0)
    for (v <- t) {
      val c = counts(v)
      if (c >= bound(v).maxCount) { return false }
      counts += v -> (c + 1)
    }

    counts.forall { case (v, c) => c >= bound(v).minCount }
  }

  private def filter(ps: ProblemState, value: Int, q: Queue[Variable]): ProblemState = {

    scope.foldLeft(ps) { (ps, v) =>
      val d = ps.dom(v)
      if (d.size > 1) {
        val nd = d.remove(value)
        if (nd.isAssigned) q.enqueue(v)
        ps.updateDomNonEmpty(v, d)
      } else ps
    }

  }

  private def upper(ps: ProblemState): Outcome = {
    val queue = new collection.mutable.Queue[Variable]()
    for (v <- scope) {
      if (ps.dom(v).size == 1) {
        queue.enqueue(v)
      }
    }

    //val singles: MultiMap[Int, Variable] = new HashMap[Int, collection.mutable.Set[Variable]] with MultiMap[Int, Variable]
    Arrays.fill(singles, 0)

    var ch = ps
    do {
      while (queue.nonEmpty) {
        singles(ps.dom(queue.dequeue).head) += 1
      }

      for (v <- singles.indices) {
        if (singles(v) > bound(v).maxCount) {
          return Contradiction
        } else if (singles(v) == bound(v).maxCount) {
          ch = filter(ch, v, queue)
        }
      }
    } while (queue.nonEmpty)
    ch
  }

  private def assignAll(ps: ProblemState, value: Int): ProblemState = {
    scope.foldLeft(ps) { (ps, v) =>
      val d = ps.dom(v)
      if (d.present(value)) ps.updateDomNonEmpty(v, d.assign(value)) else ps
    }
  }

  private def lower(ps: ProblemState): Outcome = {
    Arrays.fill(counts, 0)

    //    val counts = scope.iterator.flatMap(_.dom.values).foldLeft(Map[Int, Int]().withDefaultValue(0)) {
    //      (acc, v) => acc + (v -> (acc(v) + 1))
    //    }
    for (x <- scope; v <- ps.dom(x)) {
      counts(v) += 1
    }
    var ch = ps
    for (Bounds(v, min, _) <- bounds) {
      val c = counts(v)
      if (c < min) {
        return Contradiction
      } else if (c == min) {
        ch = assignAll(ch, v)
      }
    }
    ch
  }

  def revise(ps: ProblemState) = upper(ps) andThen lower

  def advise(ps: ProblemState, event: Event, p: Int) = arity * arity
  val simpleEvaluation = 3
}
