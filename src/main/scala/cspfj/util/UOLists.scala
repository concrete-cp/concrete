package cspfj.util

import scala.annotation.tailrec

object UOLists {

  @tailrec
  def addAll[A](l1: List[A], l2: List[A]): List[A] =
    if (l1 == Nil) l2
    else addAll(l1.tail, l1.head :: l2)

  def filter[A](l: List[A], f: A => Boolean) = {
    @tailrec
    def filt(toFilt: List[A], filtered: List[A]): List[A] =
      if (toFilt == Nil) filtered
      else if (f(toFilt.head)) filt(toFilt.tail, toFilt.head :: filtered)
      else filt(toFilt.tail, filtered)

    filt(l, Nil)
  }

}