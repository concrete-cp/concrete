package cspfj.constraint.extension;

import cspfj.util.IntTupleSet;
import scala.collection.JavaConversions

final class TupleSet(private var tupleSet: IntTupleSet, val initialContent: Boolean)
  extends Matrix with Iterable[Array[Int]] {

  def this(nbTuples: Int, initialContent: Boolean) =
    this(new IntTupleSet(nbTuples), initialContent)

  def this(initialContent: Boolean) = this(new IntTupleSet, initialContent)

  override def copy: Matrix = {
    val copy = super.clone.asInstanceOf[TupleSet]
    copy.tupleSet = new IntTupleSet(tupleSet)
    copy
  }

  override def check(tuple: Array[Int]) =
    tupleSet.containsTuple(tuple) ^ initialContent;

  override def set(tuple: Array[Int], status: Boolean) {
    if (status == initialContent) {
      tupleSet.remove(tuple)
    } else {
      tupleSet.add(tuple.clone)
    }
  }

  override def isEmpty = tupleSet.isEmpty && !initialContent;

  override def size = tupleSet.size

  def iterator = JavaConversions.asScalaIterator(mutableIterator)
  
  def mutableIterator = tupleSet.iterator

  override def toString =
    (if (initialContent) "nogoods: " else "goods: ") + tupleSet.toString

}
