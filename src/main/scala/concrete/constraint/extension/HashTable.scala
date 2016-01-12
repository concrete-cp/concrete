package concrete.constraint.extension

import cspom.util.BitVector
import java.util.Arrays
import scala.annotation.tailrec
import concrete.Variable
import concrete.Domain
import concrete.util.ArraySet

object HashTable extends RelationGenerator {
  def apply(data: Iterable[Seq[Int]]): HashTable = {
    val d = data.foldLeft(ArraySet.empty[Int])(_ + _.toArray)
    new HashTable(data.headOption.getOrElse(Seq()).size, d)
  }
}

final class HashTable(arity: Int, val table: ArraySet[Int]) extends Relation {
  type Self2 = HashTable

  def this(arity: Int) = this(arity, ArraySet.empty)

  def copy = this

  def +(t: Seq[Int]) = {
    require(t.length == arity)
    new HashTable(arity, table + t.toArray)
  }

  override def ++(t: Iterable[Seq[Int]]) = {
    require(t.forall(_.length == arity))
    new HashTable(arity, t.foldLeft(table)(_ + _.toArray))
  }

  def -(t: Seq[Int]) = throw new UnsupportedOperationException

  def filterTrie(doms: Array[Domain], modified: List[Int]) = {
    var nt = ArraySet.empty[Int]
    for (tuple <- table) {
      if (valid(modified, doms, tuple)) {
        nt += tuple
      }
    }
    if (nt.size == table.size) {
      this
    } else {
      new HashTable(arity, nt)
    }
  }

  @tailrec
  private def valid(modified: List[Int], doms: Array[Domain], t: Array[Int]): Boolean = {
    modified.isEmpty || (doms(modified.head).present(t(modified.head)) && valid(modified.tail, doms, t))
  }

  def fillFound(f: (Int, Int) => Boolean, arity: Int) = {
    val pos = new MutableList(arity)
    pos.refill()

    for (tuple <- table) {
      var pi = pos.nb - 1
      while (pi >= 0) {
        val p = pos(pi)
        if (f(p, tuple(p))) pos.remove(pi)
        pi -= 1
      }
    }
    pos.toSet
  }

  override def toString = s"${table.size} tuples"

  def edges = arity * table.size

  def find(f: (Int, Int) => Boolean) = throw new UnsupportedOperationException
  def findSupport(scope: IndexedSeq[Domain], p: Int, i: Int) = {
    table.find(t =>
      t(p) == i && (0 until arity).forall(i => scope(i).present(t(i))))
  }

  def iterator = table.iterator.map(_.toArray)

  def contains(t: Array[Int]): Boolean = table(t)

  def universal(scope: IndexedSeq[Domain]): Boolean = ???

  override def size = table.size
  def lambda = table.size
}

