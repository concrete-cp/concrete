package concrete.util

import scala.collection.generic.ImmutableMapFactory
import scala.collection.immutable.MapLike

import IdentityMap.{Wrapper, wrap}

class IdentityMap[A, +B] private(underlying: Map[Wrapper[A], B])
  extends Map[A, B] with MapLike[A, B, IdentityMap[A, B]] {

  def +[B1 >: B](kv: (A, B1)) =
    new IdentityMap(underlying + ((wrap(kv._1), kv._2)))

  def -(key: A) =
    new IdentityMap(underlying - wrap(key))

  def iterator =
    underlying.iterator.map {
      case (kw, v) => (kw.value, v)
    }

  def get(key: A) =
    underlying.get(wrap(key))

  override def size: Int =
    underlying.size

  override def empty =
    new IdentityMap(underlying.empty)

  override def stringPrefix =
    "IdentityMap"
}

object IdentityMap extends ImmutableMapFactory[IdentityMap] {
  def empty[A, B] =
    new IdentityMap(Map.empty)

  private class Wrapper[A](val value: A) {
    override def toString: String =
      value.toString

    override def equals(other: Any) = other match {
      case otherWrapper: Wrapper[_] =>
        value.asInstanceOf[AnyRef] eq otherWrapper.value.asInstanceOf[AnyRef]
      case _ => false
    }

    override def hashCode =
      System.identityHashCode(value)
  }

  private def wrap[A](key: A) =
    new Wrapper(key)
}