package cspfj.util

import scala.annotation.tailrec
import scala.collection.generic.GenericTraversableTemplate

trait UOList[+A] extends Iterable[A] {
  def head: A
  def rest: UOList[A]
  override def tail = rest
  def isEmpty: Boolean

  def +[B >: A](e: B) = UOL(e, this)

  def ++[B >: A](s: UOList[B]) = {
    @tailrec
    def add(l: UOList[B], added: UOList[B]): UOList[B] =
      if (l.isEmpty) added
      else add(l.rest, UOL(l.head, added))

    add(s, this)
  }

  override def filter(f: A => Boolean): UOList[A] = {
    @tailrec
    def filt(l: UOList[A], filtered: UOList[A]): UOList[A] =
      if (l.isEmpty) filtered
      else if (f(l.head)) filt(l.rest, filtered + l.head)
      else filt(l.rest, filtered)

    filt(this, EmptyUOList)
  }

  def iterator = new Iterator[A] {
    var current = UOList.this
    def hasNext = !current.isEmpty
    def next() = {
      val r = current.head
      current = current.rest
      r
    }
  }

  override def partition(f: A => Boolean): (UOList[A], UOList[A]) = {
    @tailrec
    def part(l: UOList[A], l0: UOList[A], l1: UOList[A]): (UOList[A], UOList[A]) = {
      if (l.isEmpty) (l0, l1)
      else if (f(l.head)) part(l.tail, l0 + l.head, l1)
      else part(l.tail, l0, l1 + l.head)
    }

    part(this, EmptyUOList, EmptyUOList)
  }

  def flatMap[B >: A](f: B => UOList[B]) = {
    @tailrec
    def fm(l: UOList[B], flat: UOList[B]): UOList[B] = {
      if (l.isEmpty) flat
      else fm(l.tail, flat ++ f(l.head))
    }

    fm(this, EmptyUOList)
  }
}

final case object EmptyUOList extends UOList[Nothing] {
  override def isEmpty = true
  override def head: Nothing =
    throw new NoSuchElementException("head of empty list")
  def rest: UOList[Nothing] =
    throw new UnsupportedOperationException("tail of empty list")
  // Removal of equals method here might lead to an infinite recursion similar to IntMap.equals.
  override def equals(that: Any) = that match {
    case that1: collection.Seq[_] => that1.isEmpty
    case _ => false
  }
}

final case class UOL[A](
  override val head: A,
  override val rest: UOList[A]) extends UOList[A] {
  override def isEmpty = false
}

object UOList {
  def empty = EmptyUOList
}

//  @tailrec
//  def addAll[A](l1: List[A], l2: List[A]): List[A] =
//    if (l1 == Nil) l2
//    else addAll(l1.tail, l1.head :: l2)
//
//  def filter[A](l: List[A], f: A => Boolean) = {
//    @tailrec
//    def filt(toFilt: List[A], filtered: List[A]): List[A] =
//      if (toFilt == Nil) filtered
//      else if (f(toFilt.head)) filt(toFilt.tail, toFilt.head :: filtered)
//      else filt(toFilt.tail, filtered)
//
//    filt(l, Nil)
//  }
//
//}