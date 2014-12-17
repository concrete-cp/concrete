package concrete.constraint

import scala.Array.canBuildFrom
import scala.annotation.implicitNotFound
import scala.annotation.tailrec
import cspom.Statistic
import concrete.Domain
import concrete.ProblemState

object TupleEnumerator {
  @Statistic
  var checks = 0L

  def clearStats() { checks = 0 }
}

trait TupleEnumerator extends Constraint {

  private def firstTuple(ps: ProblemState): Array[Int] = {
    val tuple = new Array[Int](arity)
    var i = arity - 1
    while (i >= 0) {
      tuple(i) = ps.dom(scope(i)).head
      i -= 1
    }
    tuple
  }

  @tailrec
  private def nextTuple(ps: ProblemState, t: Array[Int], p: Int): Boolean =
    if (p < 0) {
      false
    } else {
      val domain = ps.dom(scope(p))
      if (t(p) >= domain.last) {
        t(p) = domain.head
        nextTuple(ps, t, p - 1)
      } else {
        t(p) = domain.next(t(p))
        true
      }
    }

  def tuples(ps: ProblemState) = new Traversable[Array[Int]] {
    def foreach[U](f: Array[Int] => U) {
      val tuple = firstTuple(ps)
      var continue = true
      while (continue) {
        f(tuple)
        continue = nextTuple(ps, tuple, arity - 1)
      }
    }
  }

  private def firstTuple(ps: ProblemState, pos: Int, fix: Int) = {
    val t = new Array[Int](arity)
    var i = arity - 1
    while (i >= 0) {
      if (pos == i) {
        t(i) = fix
      } else {
        t(i) = ps.dom(scope(i)).head
      }
      i -= 1
    }
    t
  }

  @tailrec
  private def nextTuple(ps: ProblemState, t: Array[Int], skip: Int, p: Int): Boolean =
    if (p < 0) {
      false
    } else if (p == skip) {
      nextTuple(ps, t, skip, p - 1)
    } else {
      val domain = ps.dom(scope(p))
      if (t(p) >= domain.last) {
        t(p) = domain.head
        nextTuple(ps, t, skip, p - 1)
      } else {
        t(p) = domain.next(t(p))
        true
      }
    }

  def tuples(ps: ProblemState, pos: Int, fix: Int) = new Traversable[Array[Int]] {
    def foreach[U](f: Array[Int] => U) {
      val tuple = firstTuple(ps, pos, fix)
      var continue = true
      while (continue) {
        f(tuple)
        continue = nextTuple(ps, tuple, pos, arity - 1)
      }
    }

    override def find(p: Array[Int] => Boolean): Option[Array[Int]] = {
      val tuple = firstTuple(ps, pos, fix)
      var continue = true
      while (continue) {
        if (p(tuple)) return Some(tuple)
        continue = nextTuple(ps, tuple, pos, arity - 1)
      }
      None
    }

  }

  private def firstTuple(ps: ProblemState, base: Array[Option[Int]]): Array[Int] = {
    val t = new Array[Int](arity)
    for (i <- t.indices) {
      t(i) = base(i).getOrElse(ps.dom(scope(i)).head)
    }
    t
  }

  @tailrec
  private def nextTuple(ps: ProblemState, t: Array[Int], base: Array[Option[Int]], p: Int): Boolean =
    if (p < 0) {
      false
    } else if (base(p).isDefined) {
      nextTuple(ps, t, base, p - 1)
    } else {
      val domain = ps.dom(scope(p))
      if (t(p) >= domain.last) {
        t(p) = domain.head
        nextTuple(ps, t, base, p - 1)
      } else {
        t(p) = domain.next(t(p))
        true
      }
    }

  def tuples(ps: ProblemState, base: Array[Option[Int]]) = new Traversable[Array[Int]] {
    def foreach[U](f: Array[Int] => U) {
      val tuple = firstTuple(ps, base)
      var continue = true
      while (continue) {
        f(tuple)
        continue = nextTuple(ps, tuple, base, arity - 1)
      }
    }
  }

  def findSupport(ps: ProblemState, variablePosition: Int, index: Int): Option[Array[Int]] =
    tuples(ps, variablePosition, index).find { t =>
      TupleEnumerator.checks += 1
      check(t)
    }

  def getEvaluation(ps: ProblemState) = {
    val card = cardSize(ps)
    if (card < 0 || arity > Int.MaxValue / card) {
      Int.MaxValue
    } else {
      arity * card
    }
  }

  def simpleEvaluation = math.min(7, arity)

}