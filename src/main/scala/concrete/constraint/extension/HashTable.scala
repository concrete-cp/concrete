package concrete.constraint.extension

import java.util

import concrete.Domain
import concrete.util.ArraySet

import scala.annotation.tailrec
import scala.math.BigInt.int2bigInt

object HashTable {
  def apply(data: Seq[Array[Int]]): HashTable = {
    val d = ArraySet.empty[Int] ++ data
    new HashTable(d)
  }
}

final class HashTable(val table: ArraySet[Int]) extends Relation {
  type Self2 = HashTable


  def copy = this

  def +(t: Seq[Int]) = {
    assert(t.length == depth)
    new HashTable(table + t.toArray)
  }

  override def ++(t: Iterable[Seq[Int]]) = {
    assert(t.forall(_.length == depth))
    new HashTable(t.foldLeft(table)(_ + _.toArray))
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
      new HashTable(nt)
    }
  }

  def supported(domains: Array[Domain]): Array[util.HashSet[Int]] = {
    val arity = domains.length
    val newDomains = Array.fill(arity)(new util.HashSet[Int])

    val pos = new MutableList(arity)
    pos.refill()

    for (tuple <- table) {
      pos.filter { p =>
        ReduceableExt.fills += 1
        newDomains(p).add(tuple(p))
        newDomains(p).size != domains(p).size
      }
    }

    newDomains

  }

  override def toString = s"${table.size} tuples"

  def edges = depth * table.size

  def depth = table.headOption.map(_.size).getOrElse(0)

  def find(f: (Int, Int) => Boolean) = throw new UnsupportedOperationException

  def findSupport(scope: Array[Domain], p: Int, i: Int) = {
    table.find(t =>
      t(p) == i && t.indices.forall(i => scope(i).contains(t(i))))
  }

  def iterator = table.iterator

  def contains(t: Array[Int]): Boolean = table(t)

  def universal(scope: IndexedSeq[Domain]): Boolean = ???

  override def size = table.size

  def lambda = table.size

  @tailrec
  private def valid(modified: List[Int], doms: Array[Domain], t: Array[Int]): Boolean = {
    modified.isEmpty || (doms(modified.head).contains(t(modified.head)) && valid(modified.tail, doms, t))
  }
}

