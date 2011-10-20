package cspfj.constraint.extension;

import cspfj.problem.Variable;
import java.util.Arrays

final class MatrixManagerDynamic(
  scope: Array[Variable],
  private var tupleSet: TupleSet,
  shared: Boolean,
  tuple: Array[Int]) extends AbstractMatrixManager(scope, tupleSet, shared, tuple) with Iterable[Array[Int]] {

  private var tupleList = tupleSet.iterator.toArray

  private var listHead: Int = -1

  private var next = new Array[Int](tupleSet.size)
  Arrays.fill(next, -1)

  private var removed = new Array[Int](scope.size)
  Arrays.fill(removed, -1)

  private var removedLast = removed.clone

  for ((v, i) <- tupleList.zipWithIndex if v != null && v.size == scope.size) {
    addCell(i)
  }

  private def expandRemoved(newLength: Int) {

    val oldLength = removed.length;
    removed = Arrays.copyOf(removed, newLength);
    Arrays.fill(removed, oldLength, removed.length, -1);

    removedLast = Arrays.copyOf(removedLast, newLength);
  }

  var _level = 0;

  def level = _level

  def level_=(newLevel: Int) {
    if (newLevel < level) {
      for (i <- removed.indices if removed(i) >= 0) {
        addAll(removed(i), removedLast(i))
        removed(i) = -1
      }
    }
    _level = newLevel
  }

  private def addAll(index: Int, last: Int) {

    next(last) = listHead;
    listHead = index;

  }

  private def addCell(index: Int) {
    next(index) = listHead
    listHead = index;
  }

  def iterator = new LLIterator()

  def hsIterator = tupleSet.mutableIterator

  override def unshareMatrix = {
    tupleSet = super.unshareMatrix.asInstanceOf[TupleSet]
    matrix
  }

  override def copy = {
    def list = super.clone.asInstanceOf[MatrixManagerDynamic]
    list.next = next.clone();
    list.removed = removed.clone();
    list.removedLast = removedLast.clone();
    list;
  }

  final class LLIterator extends Iterator[Array[Int]] {

    private var current = -1;

    private var prev = -1;

    private var nextOne = listHead;

    def hasNext = nextOne >= 0

    def next() = {

      prev = current;
      current = nextOne;

      if (current >= 0) {
        nextOne = MatrixManagerDynamic.this.next(current)
      }

      tupleList(current)
    }

    def remove() { remove(level) }

    def remove(level: Int) {
      if (prev < 0) {
        listHead = nextOne;
      } else {
        next(prev) = nextOne;
      }

      if (level >= 0) {

        // assert count() == count - 1 : count + "->" + count();

        if (level >= removed.length) {
          expandRemoved(level + 1);
        }

        val oldFirstRemoved = removed(level);

        next(current) = oldFirstRemoved;
        removed(level) = current;

        if (oldFirstRemoved < 0) {
          removedLast(level) = current;
        }
      }
      current = prev;
    }
  }

  def getSize = tupleSet.size

}
