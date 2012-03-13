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
  with Backtrackable[(List[Array[Int]], Int)] {

  private var allTuples: List[Array[Int]] = tupleSet.toList

  private var _size: Int = tupleSet.size

  def save = (allTuples, size)

  def restore(d: (List[Array[Int]], Int)) {
    allTuples = d._1
    _size = d._2
  }

  override def unshareMatrix = {
    tupleSet = super.unshareMatrix.asInstanceOf[TupleSet]
    matrix
  }

  def size = _size

  override def copy = throw new UnsupportedOperationException

  def filterTuples(f: Array[Int] => Boolean) {
    val oldSize = size
    allTuples = allTuples.filter(f)
    _size = allTuples.length
    if (size != oldSize) altering()
  }
  
  def tuples = allTuples

  override def toString = "MatrixManagerDynamic managing " + size + " tuples"

}
