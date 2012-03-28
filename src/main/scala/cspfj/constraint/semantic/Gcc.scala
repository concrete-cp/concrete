package cspfj.constraint.semantic;

import scala.collection.immutable.Queue
import scala.collection.mutable.HashMap
import scala.collection.mutable.MultiMap
import cspfj.constraint.Constraint
import cspfj.problem.Variable
import cspfj.UNSATException

final case class Bounds(val value: Int, val minCount: Int, val maxCount: Int) {
  override def toString = value + ": [" + minCount + ", " + maxCount + "]"
}

final class Gcc(scope: Array[Variable], _bounds: Array[Bounds]) extends Constraint(scope) {

  var queue: Queue[Variable] = Queue.empty

  val bounds = _bounds map { b => b.value -> b } toMap

  require(scope.forall(v => v.dom.values.forall(bounds.contains)))

  def checkValues(t: Array[Int]): Boolean = {
    var counts: Map[Int, Int] = Map.empty.default(0)
    for (v <- t) {
      val c = counts(v)
      if (c >= bounds(v).maxCount) return false
      counts += v -> (c + 1)
    }

    counts.forall { case (v, c) => c > bounds(v).minCount }
  }

  private def filter(except: collection.mutable.Set[Variable], value: Int): Boolean = {
    var ch = false
    for (v <- scope if !except.contains(v)) {
      val index = v.dom.index(value)
      if (index >= 0 && v.dom.present(index)) {

        v.dom.remove(index)
        ch = true
        if (v.dom.size == 1) {
          queue = queue.enqueue(v)
        }

      }
    }
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

  def revise(): Boolean = {
    /**
     * Upper bounds
     */
    queue = Queue.empty.enqueue(scope.filter(_.dom.size == 1).toList)

    val singles: MultiMap[Int, Variable] = new HashMap[Int, collection.mutable.Set[Variable]] with MultiMap[Int, Variable]

    var ch = false

    while (queue != Nil) {
      val (checkedVariable, newQueue) = queue.dequeue
      queue = newQueue
      val value = checkedVariable.dom.firstValue

      singles.addBinding(value, checkedVariable)

      val currentSingles = singles(value)

      if (currentSingles.size == bounds(value).maxCount) {
        ch |= filter(currentSingles, value)
      } else if (currentSingles.size > bounds(value).maxCount) {
        throw UNSATException.e
      }
    }

    /**
     * Lower bounds
     */
    val counts = scope.iterator.map(_.dom.values).flatten.foldLeft(Map[Int, Int]().withDefaultValue(0)) {
      (acc, v) => acc + (v -> (acc(v) + 1))
    }

    for (b <- bounds.values) {
      if (counts(b.value) < b.minCount) throw UNSATException.e
      if (counts(b.value) == b.minCount) {
        ch |= assignAll(b.value)
      }
    }

    ch;
  }

  val getEvaluation = arity * arity
  val simpleEvaluation = 3
}
