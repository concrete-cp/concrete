package concrete
package constraint
package extension

import java.util

import scala.math.BigInt.int2bigInt
import concrete.util.ArraySet
import cspom.util.VecMap

object IndexedTable {
  def apply(data: Seq[Array[Int]]): IndexedTable = {
    val arity = data.headOption.map(_.length).getOrElse(0)
    new IndexedTable(arity) ++ data.map(_.toSeq)
  }
}

final class IndexedTable(val tables: Array[VecMap[ArraySet[Int]]]) extends Relation {
  type Self2 = IndexedTable

  def this(arity: Int) = this(Array.fill(arity)(new VecMap[ArraySet[Int]]()))

  def copy = this

  def +(t: Seq[Int]) = {
    require(t.length == tables.length)
    for (p <- t.indices) {
      tables(p)(t(p)) = tables(p).getOrElse(t(p), ArraySet.empty) + t.toArray
    }
    this
  }

  override def ++(t: Iterable[Seq[Int]]) = {
    t.foldLeft(this)(_ + _)
  }

  def -(t: Seq[Int]) = throw new UnsupportedOperationException

  def filterTrie(doms: Array[Domain], modified: List[Int]) = ???

  def supported(domains: Array[Domain]): Array[util.HashSet[Int]] = ???

  override def toString = s"indexed table"

  lazy val edges:Int = size * tables.length + tables.map(_.size).sum

  def find(f: (Int, Int) => Boolean) = throw new UnsupportedOperationException
  def findSupport(scope: Array[Domain], p: Int, i: Int) = {
    tables(p).get(i).flatMap { m =>
      m.find(t =>
        tables.indices.forall(q => q == p || scope(q).contains(t(q))))
    }
  }

  private lazy val flattened: ArraySet[Int] = {
    var as = ArraySet.empty[Int]
    for (v <- tables; i <- v) {
      as = i._2.foldLeft(as)(_ + _)
    }
    as
  }

  def iterator = flattened.iterator

  def contains(t: Array[Int]): Boolean = {
    tables(0).get(t(0)).exists(_(t))
  }

  def universal(scope: IndexedSeq[Domain]): Boolean = ???

  override def size = flattened.size
  def lambda = flattened.size
  def depth = ???
}

