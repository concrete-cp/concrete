package concrete.constraint.extension

import cspom.util.BitVector
import java.util.Arrays
import concrete.Variable
import concrete.Domain

trait Relation extends Iterable[Array[Int]] {
  type Self2 <: Relation
  def filterTrie(f: (Int, Int) => Boolean, modified: List[Int]): Self2

  /**
   * @param f(depth, i) : function called while traversing the trie, given depth and index. Returns true if traversing can be stopped at given level.
   * @param arity : arity of the relation
   * @return traversable of depths at which some values were not found
   */
  def fillFound(f: (Int, Int) => Boolean, arity: Int): Set[Int]

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

}