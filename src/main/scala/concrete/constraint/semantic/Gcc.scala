package concrete.constraint.semantic;

import scala.collection.mutable.HashMap
import scala.collection.mutable.MultiMap
import concrete.constraint.Constraint
import concrete.Variable
import concrete.UNSATException
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

  val counts = Array.ofDim[Int](1 + scope.map(_.dom.values.max).max)
  val singles = Array.ofDim[Int](1 + scope.map(_.dom.values.max).max)

  def checkValues(t: Array[Int]): Boolean = {

    var counts: Map[Int, Int] = Map.empty.withDefaultValue(0)
    for (v <- t) {
      val c = counts(v)
      if (c >= bound(v).maxCount) { return false }
      counts += v -> (c + 1)
    }

    counts.forall { case (v, c) => c >= bound(v).minCount }
  }

  private def filter(value: Int, q: Queue[Variable]) = {
    var ch: List[Int] = Nil
    for (p <- scope.indices) {
      val v = scope(p)
      if (v.dom.size > 1 && v.dom.removeVal(value)) {
        ch ::= p
        if (v.dom.size == 1) {
          q.enqueue(v)
        }
      }
    }
    ch
  }

  private def upper() = {
    val queue = new collection.mutable.Queue[Variable]()
    for (v <- scope) {
      if (v.dom.size == 1) {
        queue.enqueue(v)
      }
    }

    //val singles: MultiMap[Int, Variable] = new HashMap[Int, collection.mutable.Set[Variable]] with MultiMap[Int, Variable]
    Arrays.fill(singles, 0)

    var ch: List[Int] = Nil
    do {
      while (queue.nonEmpty) {
        singles(queue.dequeue.dom.firstValue) += 1
      }

      for (v <- singles.indices) {
        if (singles(v) > bound(v).maxCount) {
          throw UNSATException
        } else if (singles(v) == bound(v).maxCount) {
          ch ++= filter(v, queue)
        }
      }
    } while (queue.nonEmpty)
    ch
  }

  private def assignAll(value: Int): List[Int] = {
    var mod: List[Int] = Nil
    for (p <- scope.indices) {
      val v = scope(p)
      val index = v.dom.index(value);
      if (index >= 0 && v.dom.present(index)) {
        mod ::= p
        v.dom.setSingle(index)
      }
    }
    mod
  }

  private def lower() = {
    var ch: List[Int] = Nil //false
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
        throw UNSATException
      } else if (c == min) {
        ch ++= assignAll(v)
      }
    }
    ch
  }

  def revise() = (upper() ++ lower()).distinct

  def advise(p: Int) = arity * arity
  val simpleEvaluation = 3
}
