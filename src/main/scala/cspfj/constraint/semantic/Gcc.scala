package cspfj.constraint.semantic;

import scala.collection.mutable.HashMap
import scala.collection.mutable.MultiMap
import cspfj.constraint.Constraint
import cspfj.Variable
import cspfj.UNSATException
import java.util.Arrays
import scala.collection.mutable.Queue

final case class Bounds(val value: Int, val minCount: Int, val maxCount: Int) {
  override def toString = value + ": [" + minCount + ", " + maxCount + "]"
}

final class Gcc(scope: Array[Variable], _bounds: Array[Bounds]) extends Constraint(scope) {

  val offset = _bounds.map(_.value).min

  val _bounds2 = new Array[Bounds](_bounds.map(_.value).max - offset + 1)
  for (b <- _bounds) {
    _bounds2(b.value - offset) = b
  }

  def bound(v: Int) = _bounds2(v - offset)

  require(scope.forall(v => v.dom.values.forall(i => bound(i) ne null)))

  val counts = Array.ofDim[Int](1 + scope.map(_.dom.values.max).max)
  val singles = Array.ofDim[Int](1 + scope.map(_.dom.values.max).max)

  def checkValues(t: Array[Int]): Boolean = {

    var counts: Map[Int, Int] = Map.empty.withDefaultValue(0)
    for (v <- t) {
      val c = counts(v)
      if (c >= bound(v).maxCount) return false
      counts += v -> (c + 1)
    }

    counts.forall { case (v, c) => c >= bound(v).minCount }
  }

  private def filter(value: Int, q: Queue[Variable]): Boolean = {
    var ch = false
    for (v <- scope) {
      if (v.dom.size > 1 && v.dom.removeVal(value)) {
        ch = true
        if (v.dom.size == 1) {
          q.enqueue(v)
        }
      }
    }
    ch
  }

  def upper() = {
    val queue = new collection.mutable.Queue[Variable]()
    for (v <- scope) {
      if (v.dom.size == 1) {
        queue.enqueue(v)
      }
    }

    //val singles: MultiMap[Int, Variable] = new HashMap[Int, collection.mutable.Set[Variable]] with MultiMap[Int, Variable]
    Arrays.fill(singles, 0)

    var ch = false
    do {
      while (queue.nonEmpty) {
        singles(queue.dequeue.dom.firstValue) += 1
      }

      for (v <- singles.indices) {
        if (singles(v) > bound(v).maxCount) {
          throw UNSATException.e
        } else if (singles(v) == bound(v).maxCount) {
          ch |= filter(v, queue)
        }
      }
    } while (queue.nonEmpty)
    ch
  }

  private def assignAll(value: Int) = {
    var ch = false
    for (v <- scope) {
      val index = v.dom.index(value);
      if (index >= 0 && v.dom.present(index)) {
        ch = true
        v.dom.setSingle(index)
      }
    }
    ch
  }

  def lower() = {
    var ch = false
    Arrays.fill(counts, 0)

    //    val counts = scope.iterator.flatMap(_.dom.values).foldLeft(Map[Int, Int]().withDefaultValue(0)) {
    //      (acc, v) => acc + (v -> (acc(v) + 1))
    //    }
    for (x <- scope; v <- x.dom.values) {
      counts(v) += 1
    }

    for (Bounds(v, min, _) <- _bounds) {
      val c = counts(v)
      if (c < min) {
        throw UNSATException.e
      } else if (c == min) {
        ch |= assignAll(v)
      }
    }
    ch
  }

  def revise(): Boolean = upper() | lower()

  def advise(p: Int) = arity * arity
  val simpleEvaluation = 3
}
