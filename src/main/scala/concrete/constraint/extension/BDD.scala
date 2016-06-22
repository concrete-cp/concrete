package concrete
package constraint.extension

import scala.collection.mutable.HashMap
import concrete.util.TSCache
import cspom.extension.IdMap
import concrete.util.SetWithMax

object BDD {
  def apply(t: Traversable[List[Int]]): BDD = {
    implicit def cache = null
    t.foldLeft[BDD](BDD0)(_ + _)
  }

  def apply(mdd: MDD): BDD = {

    val map: IdMap[MDD, BDD] = new IdMap()

    def mdd2BDD(n: MDD): BDD = {
      if (n eq MDDLeaf) BDDLeaf
      else {
        map.getOrElseUpdate(n, {
          n.traverseST.toSeq.sortBy(-_._1).foldLeft[BDD](BDD0) {
            case (acc, (i, m)) => new BDDNode(i, mdd2BDD(m), acc)
          }
        })
      }
    }

    mdd2BDD(mdd)
  }

}

sealed trait BDD extends Iterable[Seq[Int]] {
  var id: Int = -1

  def +(e: List[Int]): BDD

  def reduce(): BDD = {

    val i = identify()

    val common = new Array[BDD](i + 1)
    common(0) = BDD0
    common(1) = BDDLeaf

    def step2(n: BDD): BDD = {

      val idn = n.id
      if (common(idn) == null) {
        val nt = n.asInstanceOf[BDDNode]
        common(idn) = new BDDNode(nt.index, step2(nt.child), step2(nt.sibling))
      }
      common(idn)

    }

    step2(this)

  }

  def filterTrie(ts: Int, doms: Array[Domain], modified: List[Int], depth: Int): BDD
  protected[extension] def passTrie(ts: Int, doms: Array[Domain], modified: List[Int], depth: Int): BDD
  protected[extension] def filterModifiedTrie(ts: Int, doms: Array[Domain], modified: List[Int], depth: Int): BDD

  def identify(): Int = {
    val cache = new HashMap[(Int, Int, Int), BDD]()

    def traverse(n: BDD, i: Int): Int = {
      n match {
        case nt: BDDNode if n.id < 0 =>
          val is = traverse(nt.sibling, traverse(nt.child, i))

          val idc = nt.child.id
          val ids = nt.sibling.id

          cache.get((nt.index, idc, ids)) match {
            case Some(m) =>
              n.id = m.id
            case None =>
              n.id = is + 1
          }

          cache((nt.index, idc, ids)) = n
          math.max(is, n.id)
        case _ => math.max(n.id, i)
      }
    }

    traverse(this, 1)

  }

  def lambda: BigInt = lambda(new IdMap())
  protected[extension] def lambda(map: IdMap[BDD, BigInt]): BigInt
  def contains(e: Seq[Int]): Boolean
  def edges(ts: Int): Int
  def depth(map: IdMap[BDD, Int]): Int

  def supported(ts: Int, domains: Array[Domain]): Array[IntDomain] = {
    val arity = domains.length
    val newDomains = Array.fill[IntDomain](arity)(EmptyIntDomain)

    def updNewDomains(depth: Int, i: Int) = {
      ReduceableExt.fills += 1
      newDomains(depth) |= i
      newDomains(depth).size == domains(depth).size
    }

    fillFound(ts, updNewDomains, 0, new SetWithMax(arity))

    newDomains
  }

  def fillFound(ts: Int, f: (Int, Int) => Boolean, depth: Int, l: SetWithMax): Unit

  def findSupport(ts: Int, scope: IndexedSeq[Domain], p: Int, i: Int, support: Array[Int], depth: Int): Option[Array[Int]] = {
    ???
  }
  def universal(scope: IndexedSeq[Domain], timestamp: Int): Boolean = ???

  def nodes(map: IdMap[BDD, Unit]): IdMap[BDD, Unit]

}

object BDD0 extends BDD {
  id = 0
  def iterator = Iterator.empty
  def +(e: List[Int]): BDD = e match {
    case Nil    => BDDLeaf
    case h :: t => new BDDNode(h, BDD0 + t, BDD0)
  }
  def lambda(map: IdMap[BDD, BigInt]) = 0
  def contains(e: Seq[Int]) = false
  def reduce(cache: collection.mutable.Map[BDD, BDD]) = BDD0
  def edges(ts: Int) = 0

  def filterTrie(ts: Int, doms: Array[Domain], modified: List[Int], depth: Int): BDD =
    this
  def passTrie(ts: Int, doms: Array[Domain], modified: List[Int], depth: Int): BDD =
    this
  def filterModifiedTrie(ts: Int, doms: Array[Domain], modified: List[Int], depth: Int): BDD =
    this

  def fillFound(ts: Int, f: (Int, Int) => Boolean, depth: Int, l: SetWithMax): Unit = ()
  def identify(i: Int) = id

  def nodes(map: IdMap[BDD, Unit]) = map

  def depth(map: IdMap[BDD, Int]) = 0
}

object BDDLeaf extends BDD {
  id = 1
  def iterator = Iterator(Seq())
  def +(e: List[Int]): BDD = {
    require(e.isEmpty)
    this
  }
  def lambda(map: IdMap[BDD, BigInt]) = 1
  def contains(e: Seq[Int]) = true
  def reduce(cache: collection.mutable.Map[BDD, BDD]) = BDDLeaf
  def edges(ts: Int) = 0
  def filterTrie(ts: Int, doms: Array[Domain], modified: List[Int], depth: Int): BDD = {
    this
  }
  def passTrie(ts: Int, doms: Array[Domain], modified: List[Int], depth: Int): BDD =
    this
  def filterModifiedTrie(ts: Int, doms: Array[Domain], modified: List[Int], depth: Int): BDD =
    this
  def fillFound(ts: Int, f: (Int, Int) => Boolean, depth: Int, l: SetWithMax): Unit = {
    l.clearFrom(depth)
  }

  override def isEmpty = false
  def nodes(map: IdMap[BDD, Unit]) = map
  def depth(map: IdMap[BDD, Int]) = 1
}

class BDDNode(val index: Int, val child: BDD, val sibling: BDD) extends BDD {
  assert(child.nonEmpty)

  private[extension] val cache: TSCache[BDD] = new TSCache()

  def iterator = child.iterator.map(index +: _) ++ sibling.iterator

  def +(e: List[Int]): BDD = {
    val h :: t = e
    if (index > h) {
      new BDDNode(h, BDD0 + t, this)
    } else if (index == h) {
      new BDDNode(index, child + t, sibling)
    } else {
      new BDDNode(index, child, sibling + e)
    }
  }

  def lambda(map: IdMap[BDD, BigInt]) =
    map.getOrElseUpdate(this, child.lambda(map) + sibling.lambda(map))

  def contains(e: Seq[Int]) = {
    val h +: t = e
    if (h < index) {
      false
    } else if (index == h) {
      child.contains(t)
    } else {
      sibling.contains(e)
    }
  }

  override def hashCode = ???

  override def equals(o: Any) = o match {
    case n: BDDNode => (n.index == index) && (n.child eq child) && (n.sibling eq sibling)
    case _          => false
  }

  def edges(ts: Int) = {
    cache(ts, 0,
      1 + child.edges(ts) + sibling.edges(ts))
  }

  def depth(map: IdMap[BDD, Int]) = {
    map.getOrElseUpdate(this, 1 + math.max(child.depth(map), sibling.depth(map)))
  }

  def filterTrie(ts: Int, doms: Array[Domain], modified: List[Int], depth: Int): BDD = {
    if (modified.isEmpty) {
      this
    } else if (modified.head == depth) {
      filterModifiedTrie(ts, doms, modified.tail, depth)
    } else {
      passTrie(ts, doms, modified, depth)
    }

  }

  def passTrie(ts: Int, doms: Array[Domain], modified: List[Int], depth: Int): BDD = {
    cache(ts) {
      val newChild = child.filterTrie(ts, doms, modified, depth + 1)
      val newSibling = sibling.passTrie(ts, doms, modified, depth)
      if (newChild.isEmpty) {
        newSibling
      } else if ((child eq newChild) && (sibling eq newSibling)) {
        this
      } else {
        new BDDNode(index, newChild, newSibling)
      }
    }
  }

  def filterModifiedTrie(ts: Int, doms: Array[Domain], modified: List[Int], depth: Int): BDD = {
    cache(ts) {
      val newSibling = sibling.filterModifiedTrie(ts, doms, modified, depth)
      if (doms(depth).present(index)) {
        val newChild = child.filterTrie(ts, doms, modified, depth + 1)
        if (newChild.isEmpty) {
          newSibling
        } else if ((child eq newChild) && (sibling eq newSibling)) {
          this
        } else {
          new BDDNode(index, newChild, newSibling)
        }
      } else {
        newSibling
      }
    }
  }

  def fillFound(ts: Int, f: (Int, Int) => Boolean, depth: Int, l: SetWithMax): Unit = {
    cache(ts, (), {
      if (depth <= l.max) {
        if (f(depth, index)) {
          l -= depth
        }
        child.fillFound(ts, f, depth + 1, l)
        sibling.fillFound(ts, f, depth, l)
      }
    })
  }

  override def isEmpty = false

  def nodes(map: IdMap[BDD, Unit]) = {
    if (map.contains(this)) {
      map
    } else {
      sibling.nodes(child.nodes(map += ((this, ()))))
    }
  }

}