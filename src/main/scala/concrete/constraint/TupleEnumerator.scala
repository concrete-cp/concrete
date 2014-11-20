package concrete.constraint

import scala.Array.canBuildFrom
import scala.annotation.implicitNotFound
import scala.annotation.tailrec
import cspom.Statistic
import concrete.Domain

object TupleEnumerator {
  @Statistic
  var checks = 0L

  def clearStats() { checks = 0 }
}

trait TupleEnumerator extends Constraint {

  private def firstTuple(domains: IndexedSeq[Domain]): Array[Int] = {
    val tuple = new Array[Int](arity)
    var i = arity - 1
    while (i >= 0) {
      tuple(i) = domains(i).head
      i -= 1
    }
    tuple
  }

  @tailrec
  private def nextTuple(domains: IndexedSeq[Domain], t: Array[Int], p: Int): Boolean =
    if (p < 0) {
      false
    } else if (t(p) >= domains(p).last) {
      t(p) = domains(p).head
      nextTuple(domains, t, p - 1)
    } else {
      t(p) = domains(p).next(t(p))
      true
    }

  def tuples(domains: IndexedSeq[Domain]) = new Traversable[Array[Int]] {
    def foreach[U](f: Array[Int] => U) {
      val tuple = firstTuple(domains)
      var continue = true
      while (continue) {
        f(tuple)
        continue = nextTuple(domains, tuple, arity - 1)
      }
    }
  }

  private def firstTuple(domains: IndexedSeq[Domain], pos: Int, fix: Int) = {
    val t = new Array[Int](arity)
    var i = arity - 1
    while (i >= 0) {
      if (pos == i) {
        t(i) = fix
      } else {
        t(i) = domains(i).head
      }
      i -= 1
    }
    t
  }

  @tailrec
  private def nextTuple(domains: IndexedSeq[Domain], t: Array[Int], skip: Int, p: Int): Boolean =
    if (p < 0) {
      false
    } else if (p == skip) {
      nextTuple(domains, t, skip, p - 1)
    } else {
      if (t(p) >= domains(p).last) {
        t(p) = domains(p).head
        nextTuple(domains, t, skip, p - 1)
      } else {
        t(p) = domains(p).next(t(p))
        true
      }
    }

  def tuples(domains: IndexedSeq[Domain], pos: Int, fix: Int) = new Traversable[Array[Int]] {
    def foreach[U](f: Array[Int] => U) {
      val tuple = firstTuple(domains, pos, fix)
      var continue = true
      while (continue) {
        f(tuple)
        continue = nextTuple(domains, tuple, pos, arity - 1)
      }
    }

    override def find(p: Array[Int] => Boolean): Option[Array[Int]] = {
      val tuple = firstTuple(domains, pos, fix)
      var continue = true
      while (continue) {
        if (p(tuple)) return Some(tuple)
        continue = nextTuple(domains, tuple, pos, arity - 1)
      }
      None
    }

  }

  private def firstTuple(domains: IndexedSeq[Domain], base: Array[Option[Int]]): Array[Int] = {
    val t = new Array[Int](arity)
    for (i <- t.indices) {
      t(i) = base(i).getOrElse(domains(i).head)
    }
    t
  }

  @tailrec
  private def nextTuple(domains: IndexedSeq[Domain], t: Array[Int], base: Array[Option[Int]], p: Int): Boolean =
    if (p < 0) {
      false
    } else if (base(p).isDefined) {
      nextTuple(domains, t, base, p - 1)
    } else {
      if (t(p) >= domains(p).last) {
        t(p) = domains(p).head
        nextTuple(domains, t, base, p - 1)
      } else {
        t(p) = domains(p).next(t(p))
        true
      }
    }

  def tuples(domains: IndexedSeq[Domain], base: Array[Option[Int]]) = new Traversable[Array[Int]] {
    def foreach[U](f: Array[Int] => U) {
      val tuple = firstTuple(domains, base)
      var continue = true
      while (continue) {
        f(tuple)
        continue = nextTuple(domains, tuple, base, arity - 1)
      }
    }
  }

  def findSupport(domains: IndexedSeq[Domain], variablePosition: Int, index: Int): Option[Array[Int]] =
    tuples(domains, variablePosition, index).find { t =>
      TupleEnumerator.checks += 1
      check(t)
    }

  def getEvaluation(domains: IndexedSeq[Domain]) = {
    val card = cardSize(domains)
    if (card < 0 || arity > Int.MaxValue / card) {
      Int.MaxValue
    } else {
      arity * card
    }
  }

  def simpleEvaluation = math.min(7, arity)

}