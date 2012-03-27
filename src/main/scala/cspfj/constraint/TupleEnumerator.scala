package cspfj.constraint
import scala.annotation.tailrec

object TupleEnumerator {
  var checks = 0

  def clearStats() { checks = 0 }
}

trait TupleEnumerator extends Constraint {

  private def firstTuple(): Array[Int] = scope map (_.dom.first)

  @tailrec
  private def nextTuple(t: Array[Int], p: Int): Option[Array[Int]] =
    if (p < 0) None
    else {
      val index = scope(p).dom.next(t(p))
      if (index < 0) {
        t(p) = scope(p).dom.first
        nextTuple(t, p - 1)
      } else {
        t(p) = index
        Some(t)
      }
    }

  def tuples() = new Iterator[Array[Int]] {
    var tuple: Option[Array[Int]] = Some(firstTuple())
    def hasNext = tuple.isDefined
    def next() = {
      val current = tuple.get
      tuple = nextTuple(current, arity - 1)
      current
    }
  }

  private def firstTuple(pos: Int, fix: Int) = {
    val t = new Array[Int](arity)
    for (i <- t.indices) {
      if (pos == i) t(i) = fix
      else t(i) = scope(i).dom.first
    }
    t
  }

  @tailrec
  private def nextTuple(t: Array[Int], skip: Int, p: Int): Option[Array[Int]] =
    if (p < 0) None
    else if (p == skip) nextTuple(t, skip, p - 1)
    else {
      val index = scope(p).dom.next(t(p))
      if (index < 0) {
        t(p) = scope(p).dom.first
        nextTuple(t, skip, p - 1)
      } else {
        t(p) = index
        Some(t)
      }
    }

  def tuples(pos: Int, fix: Int) = new Iterator[Array[Int]] {
    private var tuple: Option[Array[Int]] = Some(firstTuple(pos, fix))
    def hasNext = tuple.isDefined
    def next() = {
      val current = tuple.get
      tuple = nextTuple(current, pos, arity - 1)
      current
    }
  }

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
  private def nextTuple(t: Array[Int], base: Array[Int], p: Int): Option[Array[Int]] =
    if (p < 0) None
    else if (base(p) >= 0) nextTuple(t, base, p - 1)
    else {
      val index = t(p) + 1
      if (index >= scope(p).dom.maxSize) {
        t(p) = 0
        nextTuple(t, base, p - 1)
      } else {
        t(p) = index
        Some(t)
      }
    }

  def tuples(base: Array[Int]) = new Iterator[Array[Int]] {
    var tuple: Option[Array[Int]] = Some(firstTuple(base))
    def hasNext = tuple.isDefined
    def next() = {
      val current = tuple.get
      tuple = nextTuple(current, base, arity - 1)
      current
    }
  }

  def findSupport(variablePosition: Int, index: Int): Option[Array[Int]] =
    tuples(variablePosition, index).find { t =>
      TupleEnumerator.checks += 1
      checkIndices(t)
    }

  def getEvaluation = scope map (_.dom.size) product

  def simpleEvaluation = math.min(7, scope.count(_.dom.size > 1))

}