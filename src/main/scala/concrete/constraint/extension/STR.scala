package concrete.constraint.extension

import cspom.util.BitVector
import java.util.Arrays
import scala.annotation.tailrec
import concrete.Variable
import concrete.Domain

object STR extends RelationGenerator {
  def apply(data: Seq[Seq[Int]]): STR = {
    val d = data.map(_.toArray).toArray
    new STR(d.headOption.getOrElse(Array()).size, d, d.length)
  }
}

final class STR(arity: Int, val array: Array[Array[Int]], val bound: Int) extends Relation {
  type Self2 = STR

  def this(arity: Int) = this(arity, Array(), 0)

  def copy = new STR(arity, array.clone, bound)

  def +(t: Seq[Int]) = {
    require(t.length == arity)
    new STR(arity, t.toArray +: array, bound + 1)
  }

  override def ++(t: Iterable[Seq[Int]]) = {
    require(t.forall(_.length == arity))
    new STR(arity, t.map(_.toArray) ++: array, bound + t.size)
  }

  def -(t: Seq[Int]) = throw new UnsupportedOperationException

  def filterTrie(f: (Int, Int) => Boolean, modified: List[Int]) = {
    var b = bound
    var i = b - 1
    while (i >= 0) {
      if (!valid(modified, f, array(i))) {
        b -= 1
        val tmp = array(i)
        array(i) = array(b)
        array(b) = tmp
      }
      i -= 1
    }
    if (b == bound) {
      this
    } else {
      new STR(arity, array, b)
    }
  }

  @tailrec
  private def valid(modified: List[Int], f: (Int, Int) => Boolean, t: Array[Int]): Boolean = {
    modified.isEmpty || (f(modified.head, t(modified.head)) && valid(modified.tail, f, t))
  }

  private val pos: MutableList = new MutableList(arity)

  def fillFound(f: (Int, Int) => Boolean, arity: Int) = {

    pos.refill()

    var i = bound - 1
    while (i >= 0) {
      val tuple = array(i)
      var pi = pos.nb - 1
      while (pi >= 0) {
        val p = pos(pi)
        if (f(p, tuple(p))) pos.remove(pi)
        pi -= 1
      }
      i -= 1
    }
    pos.toSet
  }

  override def toString = s"$bound of ${array.size} tuples"

  def edges = if (array.isEmpty) 0 else bound * array(0).length

  def find(f: (Int, Int) => Boolean) = throw new UnsupportedOperationException
  def findSupport(scope: IndexedSeq[Domain], p: Int, i: Int) = throw new UnsupportedOperationException

  def iterator = array.iterator.take(bound)

  def contains(t: Array[Int]): Boolean = {
    val i = bound - 1
    while (i >= 0) {
      if (Arrays.equals(t, array(i))) return true
    }
    false
  }

  def universal(scope: IndexedSeq[Domain]): Boolean = {
    var card = 1.0 / size
    var i = scope.length - 1
    while (i >= 0) {
      card *= scope(i).size
      if (card > 1.0) {
        return false
      }
      i -= 1
    }
    true
  }

  override def size = bound
  def lambda = bound
}

final class MutableList(size: Int) extends Traversable[Int] {
  private val data = new Array[Int](size)

  def refill() {
    var i = size - 1
    while (i >= 0) {
      data(i) = i
      i -= 1
    }
  }

  private var _nb = size

  def apply(index: Int) = data(index)

  def remove(index: Int) {
    _nb -= 1
    data(index) = data(_nb)
  }

  def nb = _nb

  def foreach[U](f: Int => U) {
    var i = _nb - 1
    while (i >= 0) {
      f(data(i))
      i -= 1
    }
  }

  def sorted = {
    val a = toArray
    Arrays.sort(a)
    a.reverse
  }

}
