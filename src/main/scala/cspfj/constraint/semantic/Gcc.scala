package cspfj.constraint.semantic;

import scala.collection.immutable.Queue
import scala.collection.mutable.HashMap
import scala.collection.mutable.MultiMap
import cspfj.constraint.AbstractConstraint
import cspfj.filter.RevisionHandler
import cspfj.problem.Variable
import cspfj.constraint.SimpleRemovals

final case class Bounds(val value: Int, val minCount: Int, val maxCount: Int) {
  override def toString = value + ": [" + minCount + ", " + maxCount + "]"
}

final class Gcc(scope: Array[Variable], _bounds: Array[Bounds]) extends AbstractConstraint(scope)
  with SimpleRemovals {

  var queue: Queue[Variable] = Queue.empty

  val bounds = _bounds map { b => b.value -> b } toMap

  require(scope.forall(v => v.dom.values.forall(bounds.contains)))

  def check: Boolean = {
    var counts: Map[Int, Int] = Map.empty.default(0)
    for (v <- tupleValues) {
      val c = counts(v)
      if (c >= bounds(v).maxCount) return false
      counts += v -> (c + 1)
    }

    counts.forall { case (v, c) => c > bounds(v).minCount }

  }

  private def filter(except: collection.mutable.Set[Variable], value: Int, revisator: RevisionHandler): Boolean = {
    for (v <- scope if !except.contains(v)) {
      val index = v.dom.index(value)
      if (index >= 0 && v.dom.present(index)) {
        if (v.dom.size == 1) {
          return true
        }

        v.dom.remove(index)

        if (v.dom.size == 1) {
          queue = queue.enqueue(v)
        }
        revisator.revised(this, v)

      }
    }
    false;
  }

  private def assignAll(value: Int, revisator: RevisionHandler) {
    for (v <- scope) {
      val index = v.dom.index(value);
      if (index >= 0 && v.dom.present(index)) {
        v.dom.setSingle(index)
        revisator.revised(this, v)
      }
    }
  }

  def revise(revisator: RevisionHandler, reviseCount: Int): Boolean = {
    /**
     * Upper bounds
     */
    queue = Queue.empty.enqueue(scope.filter(_.dom.size == 1).toList)

    val singles: MultiMap[Int, Variable] = new HashMap[Int, collection.mutable.Set[Variable]] with MultiMap[Int, Variable]

    while (queue != Nil) {
      val (checkedVariable, newQueue) = queue.dequeue
      queue = newQueue
      val value = checkedVariable.dom.firstValue

      singles.addBinding(value, checkedVariable)

      val currentSingles = singles(value)

      if (currentSingles.size == bounds(value).maxCount) {
        if (filter(currentSingles, value, revisator)) {
          return false;
        }
      } else if (currentSingles.size > bounds(value).maxCount) {
        return false;
      }
    }

    /**
     * Lower bounds
     */
    val counts = scope.iterator.map(_.dom.values).flatten.foldLeft(Map[Int, Int]().withDefaultValue(0)) {
      (acc, v) => acc + (v -> (acc(v) + 1))
    }

    for (b <- bounds.values) {
      if (counts(b.value) < b.minCount) return false
      if (counts(b.value) == b.minCount) assignAll(b.value, revisator)
    }

    true;
  }

  val getEvaluation = arity.doubleValue * arity

}
