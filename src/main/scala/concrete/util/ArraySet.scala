package concrete.util

import scala.collection.AbstractSet
import scala.collection.mutable.WrappedArray

object ArraySet {
  def empty[T] = new ArraySet[T](Set.empty)
  
}

class ArraySet[T](val set: Set[WrappedArray[T]]) extends AbstractSet[Array[T]] {

  def -(elem: Array[T]) = new ArraySet(set - elem)
  def +(elem: Array[T]) = new ArraySet(set + elem)
  def contains(elem: Array[T]) = set.contains(elem)
  def iterator = set.iterator.map(_.array)

}