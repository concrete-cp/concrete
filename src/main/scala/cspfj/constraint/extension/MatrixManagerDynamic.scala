package cspfj.constraint.extension;

import cspfj.problem.Variable
import java.util.Arrays
import scala.collection.mutable.DoubleLinkedList
import scala.annotation.tailrec

final class MatrixManagerDynamic(
  scope: Array[Variable],
  private var tupleSet: TupleSet,
  shared: Boolean,
  tuple: Array[Int]) extends AbstractMatrixManager(scope, tupleSet, shared, tuple) with Iterable[Array[Int]] {

  private var allTuples: DoubleLinkedList[Array[Int]] = {
//    @tailrec
//    def build(
//      current: Iterator[Array[Int]],
//      built: DoubleLinkedList[Array[Int]]): DoubleLinkedList[Array[Int]] = {
//      if (!current.hasNext) {
//        built
//      } else {
//        build(current, built += current.next)
//      }
//    }
//    build(tupleSet.iterator, DoubleLinkedList.empty)
    val b = DoubleLinkedList.newBuilder[Array[Int]]
    tupleSet.foreach(b +=)
    b.result
  }

  private var removedTuples: List[(Int, DoubleLinkedList[Array[Int]])] = Nil

  var _level = 0;

  def level = _level

  def level_=(newLevel: Int) {
    if (newLevel < level) {
      while (removedTuples != Nil && removedTuples.head._1 >= newLevel) {
        allTuples ++= removedTuples.head._2
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

  override def copy = {
    val c = super.clone.asInstanceOf[MatrixManagerDynamic]
    c.tupleSet = tupleSet.copy
    c.allTuples = DoubleLinkedList.empty ++ allTuples.iterator
    c
  }

  final class LLIterator extends Iterator[Array[Int]] {

    private var current = allTuples
    private var last: DoubleLinkedList[Array[Int]] = null

    def hasNext = !current.isEmpty

    def next() = {
      last = current
      current = current.next
      last.head
    }

    def remove() { remove(level) }

    def remove(level: Int) {
      assert(removedTuples.isEmpty || removedTuples.head._1 <= level)
      if (removedTuples == Nil || removedTuples.head._1 < level) {
        removedTuples ::= (level, DoubleLinkedList(last.head))
      } else {
        removedTuples.head._2 :+ last.head
      }

      if (allTuples == last) {
        allTuples = last.tail
      }
      last.remove
      last = null
    }

  }

  def getSize = tupleSet.size

  override def toString = "MatrixManagerDynamic managing " + getSize + " tuples"

}
