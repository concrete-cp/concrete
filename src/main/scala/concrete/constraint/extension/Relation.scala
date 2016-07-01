package concrete.constraint.extension

import concrete.Domain

trait Relation extends Iterable[Array[Int]] {
  type Self2 <: Relation
  def filterTrie(doms: Array[Domain], modified: List[Int]): Self2

  def supported(domains: Array[Domain]): Array[Domain]

  def contains(t: Array[Int]): Boolean
  def +(t: Seq[Int]): Self2
  def -(t: Seq[Int]): Self2
  def ++(t: Iterable[Seq[Int]]) = t.foldLeft(Relation.this)(_ + _)
  def --(t: Iterable[Seq[Int]]) = t.foldLeft(Relation.this)(_ - _)
  def edges: Int
  def copy: Self2
  def findSupport(scope: IndexedSeq[Domain], p: Int, i: Int): Option[Array[Int]]
  def lambda: BigInt
  def universal(scope: IndexedSeq[Domain]): Boolean
  def depth: Int

}