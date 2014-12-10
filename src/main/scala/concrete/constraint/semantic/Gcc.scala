package concrete.constraint.semantic;

import scala.collection.mutable.HashMap
import scala.collection.mutable.MultiMap
import concrete.constraint.Constraint
import concrete.Variable
import concrete.UNSATException
import java.util.Arrays
import scala.collection.mutable.Queue
import concrete.UNSATObject
import concrete.UNSATException

import concrete.Revised
import scala.collection.mutable.BitSet
import concrete.ReviseOutcome
import concrete.Domain
import concrete.Contradiction
import concrete.Singleton

final case class Bounds(val value: Int, val minCount: Int, val maxCount: Int) {
  override def toString = value + ": [" + minCount + ", " + maxCount + "]"
}

final class Gcc(scope: Array[Variable], bounds: Array[Bounds]) extends Constraint(scope) {
  type State = Unit
  def initState = Unit
  val offset = bounds.map(_.value).min

  val _bounds2 = new Array[Bounds](bounds.map(_.value).max - offset + 1)
  for (b <- bounds) {
    _bounds2(b.value - offset) = b
  }

  def bound(v: Int) = _bounds2(v - offset)

  val counts = Array.ofDim[Int](1 + scope.map(_.initDomain.max).max)
  val singles = Array.ofDim[Int](1 + scope.map(_.initDomain.max).max)

  def check(t: Array[Int]): Boolean = {

    var counts: Map[Int, Int] = Map.empty.withDefaultValue(0)
    for (v <- t) {
      val c = counts(v)
      if (c >= bound(v).maxCount) { return false }
      counts += v -> (c + 1)
    }

    counts.forall { case (v, c) => c >= bound(v).minCount }
  }

  private def filter(domains: IndexedSeq[Domain], value: Int, q: Queue[Int]): IndexedSeq[Domain] = {

    for (p <- domains.indices) yield {
      val d = domains(p)
      if (d.size > 1) {
        val nd = d.remove(value)
        if (nd.size == 1) q.enqueue(p)
        nd
      } else d
    }

  }

  private def upper(domains: IndexedSeq[Domain]): ReviseOutcome[Unit] = {
    val queue = new collection.mutable.Queue[Int]()
    for (d <- domains.indices) {
      if (domains(d).size == 1) {
        queue.enqueue(d)
      }
    }

    //val singles: MultiMap[Int, Variable] = new HashMap[Int, collection.mutable.Set[Variable]] with MultiMap[Int, Variable]
    Arrays.fill(singles, 0)

    var ch = domains
    do {
      while (queue.nonEmpty) {
        singles(domains(queue.dequeue).head) += 1
      }

      for (v <- singles.indices) {
        if (singles(v) > bound(v).maxCount) {
          return Contradiction
        } else if (singles(v) == bound(v).maxCount) {
          ch = filter(ch, v, queue)
        }
      }
    } while (queue.nonEmpty)
    Revised(ch)
  }

  private def assignAll(domains: IndexedSeq[Domain], value: Int): IndexedSeq[Domain] = {
    for (d <- domains) yield {
      if (d.present(value)) d.assign(value) else d
    }
  }

  private def lower(domains: IndexedSeq[Domain]): ReviseOutcome[Unit] = {
    Arrays.fill(counts, 0)

    //    val counts = scope.iterator.flatMap(_.dom.values).foldLeft(Map[Int, Int]().withDefaultValue(0)) {
    //      (acc, v) => acc + (v -> (acc(v) + 1))
    //    }
    for (x <- domains; v <- x) {
      counts(v) += 1
    }
    var ch = domains
    for (Bounds(v, min, _) <- bounds) {
      val c = counts(v)
      if (c < min) {
        return Contradiction
      } else if (c == min) {
        ch = assignAll(ch, v)
      }
    }
    Revised(ch)
  }

  def revise(domains: IndexedSeq[Domain], s: State) = upper(domains) andThen ((c, _) => lower(c))

  def advise(domains: IndexedSeq[Domain], p: Int) = arity * arity
  val simpleEvaluation = 3
}
