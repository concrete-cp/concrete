package concrete.constraint.extension

import cspom.util.BitVector
import java.util.Arrays
import scala.annotation.tailrec
import concrete.Variable
import concrete.Domain
import cspom.util.VecMap
import concrete.util.ArraySet

object IndexedTable extends RelationGenerator {
  def apply(data: Iterable[Seq[Int]]): IndexedTable = {
    val arity = data.headOption.getOrElse(Set()).size
    new IndexedTable(arity) ++ data
  }
}

final class IndexedTable(val tables: Array[VecMap[ArraySet[Int]]]) extends Relation {
  type Self2 = IndexedTable

  def this(arity: Int) = this(Array.fill(arity)(new VecMap[ArraySet[Int]]()))

  def copy = this

  def +(t: Seq[Int]) = {
    require(t.length == tables.length)
    for (p <- 0 until t.length) {
      tables(p)(t(p)) = tables(p).getOrElse(t(p), ArraySet.empty) + t.toArray
    }
    this
  }

  override def ++(t: Iterable[Seq[Int]]) = {
    t.foldLeft(this)(_ + _)
  }

  def -(t: Seq[Int]) = throw new UnsupportedOperationException

  def filterTrie(doms: Array[Domain], modified: List[Int]) = ???

  def fillFound(f: (Int, Int) => Boolean, arity: Int) = ???

  override def toString = s"indexed table"

  lazy val edges = size * tables.length + tables.map(_.size).sum

  def find(f: (Int, Int) => Boolean) = throw new UnsupportedOperationException
  def findSupport(scope: IndexedSeq[Domain], p: Int, i: Int) = {
    tables(p).get(i).flatMap { m =>
      m.find(t =>
        (0 until tables.length).forall(q => q == p || scope(q).present(t(q))))
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
}

