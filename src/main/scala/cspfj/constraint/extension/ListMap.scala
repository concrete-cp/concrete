package cspfj.constraint.extension

import scala.annotation.tailrec

object ListMap {
  def empty[A, B] = EmptyListMap.asInstanceOf[ListMap[A, B]]

  case object EmptyListMap extends ListMap[Any, Nothing] {
    override def -(k: Any) = this
    override def get(k: Any) = None
    override def isEmpty = true
    override def iterator = Iterator.empty
    override def size = 0
    override def tail: ListMap[Any, Nothing] =
      throw new UnsupportedOperationException("tail of empty list")

  }
}

trait ListMap[A, +B] extends Map[A, B] { self =>

  def +[B1 >: B](e: (A, B1)) = new ListMapNode(e, this - e._1)
  
  def -(k: A): ListMap[A, B] = {
    if (k == head._1) tail
    else ListMapNode(head, tail - k)
  }

  def get(k: A) =
    if (k == head._1) Some(head._2)
    else tail.get(k)

  def contains[B1 >: B](e: (A, B1)): Boolean = e == head || tail.contains(e)

  override def tail: ListMap[A, B] = throw new UnsupportedOperationException

  def iterator = new Iterator[(A, B)]() {
    var current = ListMap.this
    def hasNext = !current.isEmpty
    def next = {
      val c = current.head
      current = current.tail
      c
    }
  }

  @tailrec
  private def fK[B1 >: B](f: (A => Boolean), acc: ListMap[A, B1]): ListMap[A, B1] =
    if (isEmpty) acc
    else if (f(head._1)) tail.fK(f, new ListMapNode(head, acc))
    else tail.fK(f, acc)

  override def filterKeys(f: (A => Boolean)) = fK(f, ListMap.empty[A, B])

  @tailrec
  private def filt[B1 >: B](f: (((A, B1)) => Boolean), acc: ListMap[A, B1]): ListMap[A, B1] =
    if (isEmpty) acc
    else if (f(head)) tail.filt(f, new ListMapNode(head, acc))
    else tail.filt(f, acc)

  override def filter(f: ((A, B)) => Boolean) = filt(f, ListMap.empty[A, B])

  override def equals(o: Any): Boolean = o match {
    case l: Map[A, B] => size == l.size && l.forall(contains)
    case _ => false
  }

  private def mV[C](f: B => C, acc: ListMap[A, C]): ListMap[A, C] =
    if (isEmpty) acc
    else tail.mV(f, new ListMapNode((head._1, f(head._2)), acc))

  override def mapValues[C](f: B => C) = mV(f, ListMap.empty[A, C])

}

case class ListMapNode[A, +B](
  override val head: (A, B),
  override val tail: ListMap[A, B]) extends ListMap[A, B] {
  override def isEmpty = false

}