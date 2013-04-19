package cspfj.constraint.extension

import cspfj.util.BitVector
import java.util.Arrays

trait Relation extends Iterable[Array[Int]] {
  type Self2 <: Relation
  def filterTrie(f: (Int, Int) => Boolean, modified: List[Int]): Self2

  /**
   * @param f(depth, i) : function called while traversing the trie, given depth and index. Returns true if traversing can be stopped at given level.
   * @param arity : arity of the relation
   * @return traversable of depths at which some values were not found
   */
  def fillFound(f: (Int, Int) => Boolean, arity: Int): Traversable[Int]

  def contains(t: Array[Int]): Boolean
  def find(f: (Int, Int) => Boolean): Option[Array[Int]]
  def +(t: Array[Int]): Self2
  def -(t: Array[Int]): Self2
  def ++(t: Iterable[Array[Int]]) = t.foldLeft(Relation.this)(_ + _)
  def --(t: Iterable[Array[Int]]) = t.foldLeft(Relation.this)(_ - _)
  def edges: Int
  def copy: Self2
  def findSupport(f: (Int, Int) => Boolean, p: Int, i: Int, support: Array[Int]): Option[Array[Int]]
  def lambda: BigInt
  
  //  override def equals(o: Any) = {
  //    o match {
  //      case r: Iterable[Array[Int]] => size == r.size && zip(r).forall(p => Arrays.equals(p._1, p._2))
  //      case _ => false
  //    }
  //  }
}