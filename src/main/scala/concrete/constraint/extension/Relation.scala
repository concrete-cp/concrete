package concrete.constraint.extension

import java.util

import concrete.Domain

trait Relation extends Iterable[Array[Int]] {
  type Self2 <: Relation
  def filterTrie(doms: Array[Domain], modified: List[Int]): Self2

  def supported(domains: Array[Domain]): Array[util.HashSet[Int]]

  def contains(t: Array[Int]): Boolean
  def +(t: Seq[Int]): Self2
  def -(t: Seq[Int]): Self2
  def ++(t: Iterable[Seq[Int]]): Relation = t.foldLeft(Relation.this)(_ + _)
  def --(t: Iterable[Seq[Int]]): Relation = t.foldLeft(Relation.this)(_ - _)
  def edges: Int
  // def copy: Self2
  def findSupport(scope: Array[Domain], p: Int, i: Int): Option[Array[Int]]
  def lambda: BigInt
  def depth: Int

}