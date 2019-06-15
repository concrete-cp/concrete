package concrete.util

import IdentityMap.{Wrapper, wrap}

import scala.collection.immutable

class IdentityMap[A, +B] (
                           private val underlying: immutable.Map[Wrapper[A], B] = Map.empty[Wrapper[A], B])
  extends immutable.AbstractMap[A, B]  {

  def updated[B1 >: B](k: A, v: B1) =
    new IdentityMap(underlying + (wrap(k) -> v))

  def removed(key: A) =
    new IdentityMap(underlying - wrap(key))

  def iterator: Iterator[(A, B)] =
    underlying.iterator.map {
      case (kw, v) => (kw.value, v)
    }

  def get(key: A): Option[B] =
    underlying.get(wrap(key))

  override def size: Int =
    underlying.size

  override def empty =
    new IdentityMap(underlying.empty)
}

object IdentityMap  {

  class Wrapper[A](val value: A) {
    override def toString: String =
      value.toString

    override def equals(other: Any): Boolean = other match {
      case otherWrapper: Wrapper[_] =>
        value.asInstanceOf[AnyRef] eq otherWrapper.value.asInstanceOf[AnyRef]
      case _ => false
    }

    override def hashCode: Int =
      System.identityHashCode(value)
  }

  private def wrap[A](key: A) =
    new Wrapper(key)
}