package cspfj.constraint.extension;

import cspfj.problem.Variable
import java.util.Arrays
import scala.collection.mutable.DoubleLinkedList
import scala.annotation.tailrec
import cspfj.util.DLList

final class MatrixManagerDynamic(
  scope: Array[Variable],
  private var tupleSet: TupleSet,
  shared: Boolean,
  tuple: Array[Int]) extends AbstractMatrixManager(scope, tupleSet, shared, tuple) with Iterable[Array[Int]] {

  private val allTuples: DLList[Array[Int]] = new DLList()
  tupleSet.foreach(allTuples.add)

  private var removedTuples: List[(Int, DLList[Array[Int]])] = Nil

  var _level = 0;

  def level = _level

  def level_=(newLevel: Int) {
    if (newLevel < level) {
      while (removedTuples != Nil && removedTuples.head._1 >= newLevel) {
        allTuples.merge(removedTuples.head._2)
        removedTuples = removedTuples.tail
      }
    }
    _level = newLevel
  }

  def iterator = new LLIterator()

  override def unshareMatrix = {
    tupleSet = super.unshareMatrix.asInstanceOf[TupleSet]
    matrix
  }

  override def copy = throw new UnsupportedOperationException

  final class LLIterator extends Iterator[Array[Int]] {
    private val itr = allTuples.iterator

    def hasNext = itr.hasNext
    def next() = itr.next()

    def remove() { remove(level) }

    def remove(level: Int) {
      assert(removedTuples.isEmpty || removedTuples.head._1 <= level)
      val removed = itr.remove()

      val histo =
        if (removedTuples == Nil || removedTuples.head._1 < level) {
          val h: DLList[Array[Int]] = new DLList()

          removedTuples ::= (level, h)

          h
        } else {
          removedTuples.head._2
        }

      histo.addNode(removed)

    }

  }

  def getSize = tupleSet.size

  override def toString = "MatrixManagerDynamic managing " + getSize + " tuples"

}
