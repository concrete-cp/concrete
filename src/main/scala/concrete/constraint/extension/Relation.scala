package concrete.constraint.extension

import concrete.Domain
import concrete.IntDomain

trait Relation extends Iterable[Array[Int]] {
  type Self2 <: Relation
  def filterTrie(doms: Array[Domain], modified: List[Int]): Self2

  /**
   * @param f(depth, i) : function called while traversing the trie, given depth and index. Returns true if traversing can be stopped at given level.
   * @param arity : arity of the relation
   * @return traversable of depths at which some values were not found
   */
  //def fillFound(f: (Int, Int) => Boolean, arity: Int): Set[Int]

  def supported(domains: Array[Domain]): Array[IntDomain]

  //  = {
  //    val arity = domains.length
  //    val newDomains = Array.fill[IntDomain](arity)(EmptyIntDomain)
  //
  //    //print(s"Searching for supports of ${domains.toSeq}")
  //    
  //    def updNewDomains(depth: Int, i: Int) = {
  //      ReduceableExt.fills += 1
  //      //print(s"$depth: ${newDomains(depth)} | $i = ")
  //      newDomains(depth) |= i
  //      //println(newDomains(depth))
  //      newDomains(depth).length == domains(depth).length
  //    }
  //
  //    fillFound(updNewDomains, arity)
  //    //println(s" = ${newDomains.toSeq}")
  //
  //    newDomains
  //  }

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