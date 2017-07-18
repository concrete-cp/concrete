package concrete.util

import scala.collection.{AbstractSet, GenTraversableOnce}
import scala.collection.mutable.WrappedArray

object ArraySet {
  def empty[T] = new ArraySet[T](Set.empty)
  
}

class ArraySet[T](val set: Set[WrappedArray[T]]) extends AbstractSet[Array[T]] {

  def -(elem: Array[T]) = new ArraySet(set - elem)
  def +(elem: Array[T]) = new ArraySet(set + elem)

  override def ++(elems: GenTraversableOnce[Array[T]]): ArraySet[T] =
    new ArraySet(set ++ elems.toIterator.map(WrappedArray.make[T](_)))

  def contains(elem: Array[T]) = set.contains(elem)
  def iterator = set.iterator.map(_.array)

}