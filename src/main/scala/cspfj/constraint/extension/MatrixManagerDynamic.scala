package cspfj.constraint.extension;

import cspfj.problem.Variable
import java.util.Arrays
import scala.collection.mutable.DoubleLinkedList
import scala.annotation.tailrec
import cspfj.util.DLList
import cspfj.util.Backtrackable

final class MatrixManagerDynamic(
  scope: Array[Variable],
  private var tupleSet: TupleSet,
  shared: Boolean,
  tuple: Array[Int])
  extends AbstractMatrixManager(scope, tupleSet, shared, tuple)
  with Iterable[Array[Int]]
  with Backtrackable[List[Array[Int]]] {

  private var allTuples: List[Array[Int]] = tupleSet.toList

  def save = allTuples

  def restore(d: List[Array[Int]]) {
    allTuples = d
  }

  override def unshareMatrix = {
    tupleSet = super.unshareMatrix.asInstanceOf[TupleSet]
    matrix
  }

  override def copy = throw new UnsupportedOperationException

  override def size = allTuples.size

  def filterTuples(f: Array[Int] => Boolean) {
    val oldSize = size
    allTuples = allTuples.filter(f)
    if (size != oldSize) altering()
  }
  
  def iterator = allTuples.iterator

  override def toString = "MatrixManagerDynamic managing " + size + " tuples"

}
