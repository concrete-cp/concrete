package cspfj.constraint

import scala.Array.canBuildFrom
import scala.annotation.implicitNotFound
import scala.annotation.tailrec

import cspfj.Statistic

object TupleEnumerator {
  @Statistic
  var checks = 0L

  def clearStats() { checks = 0 }
}

trait TupleEnumerator extends Constraint {

  private def firstTuple(): Array[Int] = scope map (_.dom.first)

  @tailrec
  private def nextTuple(t: Array[Int], p: Int): Array[Int] =
    if (p < 0) { null }
    else {
      val index = scope(p).dom.next(t(p))
      if (index < 0) {
        t(p) = scope(p).dom.first
        nextTuple(t, p - 1)
      } else {
        t(p) = index
        t
      }
    }

  def tuples() = new Traversable[Array[Int]] {
    def foreach[U](f: Array[Int] => U) {
      var tuple = firstTuple()
      while (tuple ne null) {
        f(tuple)
        tuple = nextTuple(tuple, arity - 1)
      }
    }
  }

  private def firstTuple(pos: Int, fix: Int) = {
    val t = new Array[Int](arity)
    var i = arity - 1
    while (i >= 0) {
      if (pos == i) {
        t(i) = fix
      } else {
        t(i) = scope(i).dom.first
      }
      i -= 1
    }
    t
  }

  @tailrec
  private def nextTuple(t: Array[Int], skip: Int, p: Int): Array[Int] =
    if (p < 0) {
      null
    } else if (p == skip) {
      nextTuple(t, skip, p - 1)
    } else {
      val index = scope(p).dom.next(t(p))
      if (index < 0) {
        t(p) = scope(p).dom.first
        nextTuple(t, skip, p - 1)
      } else {
        t(p) = index
        t
      }
    }

  def tuples(pos: Int, fix: Int) = new Traversable[Array[Int]] {
    def foreach[U](f: Array[Int] => U) {
      var tuple = firstTuple(pos, fix)
      while (tuple ne null) {
        f(tuple)
        tuple = nextTuple(tuple, pos, arity - 1)
      }
    }

    override def find(p: Array[Int] => Boolean): Option[Array[Int]] = {
      var tuple = firstTuple(pos, fix)
      while (tuple ne null) {
        if (p(tuple)) return Some(tuple)
        tuple = nextTuple(tuple, pos, arity - 1)
      }
      None
    }

  }

  //  def tuples(pos: Int, fix: Int) = new Iterator[Array[Int]] {
  //    private var tuple: Array[Int] = firstTuple(pos, fix)
  //    def hasNext = tuple ne null
  //    def next() = {
  //      val current = tuple
  //      tuple = nextTuple(current, pos, arity - 1)
  //      current
  //    }
  //  }

  private def firstTuple(base: Array[Int]): Array[Int] = {
    val t = new Array[Int](arity)
    for (i <- t.indices) {
      if (base(i) >= 0) {
        t(i) = base(i)
      } else {
        t(i) = 0
      }
    }
    t
  }

  @tailrec
  private def nextTuple(t: Array[Int], base: Array[Int], p: Int): Array[Int] =
    if (p < 0) {
      null
    } else if (base(p) >= 0) {
      nextTuple(t, base, p - 1)
    } else {
      val index = t(p) + 1
      if (index >= scope(p).dom.maxSize) {
        t(p) = 0
        nextTuple(t, base, p - 1)
      } else {
        t(p) = index
        t
      }
    }

  def tuples(base: Array[Int]) = new Traversable[Array[Int]] {
    def foreach[U](f: Array[Int] => U) {
      var tuple = firstTuple(base)
      while (tuple ne null) {
        f(tuple)
        tuple = nextTuple(tuple, base, arity - 1)
      }
    }
  }

  def findSupport(variablePosition: Int, index: Int): Option[Array[Int]] =
    tuples(variablePosition, index).find { t =>
      TupleEnumerator.checks += 1
      checkIndices(t)
    }

  def getEvaluation = arity * (scope map (_.dom.size) product)

  def simpleEvaluation = math.min(7, scope.count(_.dom.size > 1))

}