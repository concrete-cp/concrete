package concrete.constraint.extension

import java.util

import concrete.Domain

import scala.annotation.tailrec
import scala.collection.immutable.ArraySeq
import scala.math.BigInt.int2bigInt

object HashTable {
  def apply(data: Array[Array[Int]]): HashTable = {
    val d = Set.empty[ArraySeq[Int]] ++ data.map(ArraySeq.unsafeWrapArray)
    new HashTable(d)
  }
}

final class HashTable(val table: Set[ArraySeq[Int]]) extends Relation {
  type Self2 = HashTable


  def copy: HashTable = this

  def +(t: Seq[Int]): HashTable = {
    assert(t.length == depth)
    new HashTable(table + ArraySeq.from(t))
  }

  override def ++(t: Iterable[Seq[Int]]): HashTable = {
    assert(t.forall(_.length == depth))
    new HashTable(t.foldLeft(table)(_ + ArraySeq.from(_)))
  }

  def -(t: Seq[Int]) = throw new UnsupportedOperationException

  def filterTrie(doms: Array[Domain], modified: List[Int]): HashTable = {
    var nt = Set.empty[ArraySeq[Int]]
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

  def edges: Int = depth * table.size

  def depth: Int = table.headOption.map(_.size).getOrElse(0)

  def find(f: (Int, Int) => Boolean) = throw new UnsupportedOperationException

  def findSupport(scope: Array[Domain], p: Int, i: Int): Option[Array[Int]] = {
    table.find(t =>
      t(p) == i && t.indices.forall(i => scope(i).contains(t(i))))
      .map(_.unsafeArray.asInstanceOf[Array[Int]])
  }

  def iterator: Iterator[Array[Int]] = table.iterator.map(_.unsafeArray.asInstanceOf[Array[Int]])

  def contains(t: Array[Int]): Boolean = table.contains(ArraySeq.unsafeWrapArray(t))

  def universal(scope: IndexedSeq[Domain]): Boolean = ???

  override def size: Int = table.size

  def lambda: BigInt = table.size

  @tailrec
  private def valid(modified: List[Int], doms: Array[Domain], t: ArraySeq[Int]): Boolean = {
    modified.isEmpty || (doms(modified.head).contains(t(modified.head)) && valid(modified.tail, doms, t))
  }
}

