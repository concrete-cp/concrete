package concrete
package constraint.extension

import scala.Iterator
import scala.collection.mutable.HashMap
import scala.math.BigInt.int2bigInt

import concrete.Domain
import concrete.IntDomain
import concrete.util.TSCache
import cspom.extension.IdMap

object BDDM {
  def apply(t: Traversable[List[Int]]): BDDM = {
    implicit def cache = null
    t.foldLeft[BDDM](BDDM0)(_ + _)
  }

  def apply(mdd: MDD): BDDM = {

    val map: IdMap[MDD, BDDM] = new IdMap()

    def mdd2BDDM(n: MDD): BDDM = {
      if (n eq MDDLeaf) BDDMLeaf
      else {
        map.getOrElseUpdate(n, {
          n.traverseST.toSeq.sortBy(-_._1).foldLeft[BDDM](BDDM0) {
            case (acc, (i, m)) => new BDDMNode(i, mdd2BDDM(m), acc)
          }
        })
      }
    }

    mdd2BDDM(mdd)
  }

}

sealed trait BDDM extends BDD {
  var id: Int = _

  override def +(e: List[Int])(implicit cache: Cache): BDDM

  def reduce(): BDDM = {

    val i = identify()

    val common = new Array[BDDM](i + 1)
    common(0) = BDDM0
    common(1) = BDDMLeaf

    def step2(n: BDDM): BDDM = {

      val idn = n.id
      if (common(idn) == null) {
        val nt = n.asInstanceOf[BDDMNode]
        common(idn) = new BDDMNode(nt.index, step2(nt.child), step2(nt.sibling))
      }
      common(idn)

    }

    step2(this)

  }
  def filterTrie(ts: Int, doms: Array[Domain], modified: List[Int], depth: Int, cache: Cache): BDDM
  protected[extension] def passTrie(ts: Int, doms: Array[Domain], modified: List[Int], depth: Int, cache: Cache): BDDM
  protected[extension] def filterModifiedTrie(ts: Int, doms: Array[Domain], modified: List[Int], depth: Int, cache: Cache): BDDM

  def identify(): Int = {
    val cache = new HashMap[(Int, Int, Int), BDDM]()

    def traverse(n: BDDM, i: Int): Int = {
      n match {
        case nt: BDDMNode =>

          val is = traverse(nt.sibling, traverse(nt.child, i))

          val idc = nt.child.id
          val ids = nt.sibling.id

          val in = cache.get((nt.index, idc, ids)) match {
            case Some(m) =>
              n.id = m.id
              is
            case None =>
              n.id = is
              is + 1
          }

          cache((nt.index, idc, ids)) = n
          in
        case e => i
      }
    }

    traverse(this, 2)
  }

  def fillFound(ts: Int, f: (Int, Int) ⇒ Boolean, depth: Int, l: concrete.util.SetWithMax): Unit = ???

  override def supported(ts: Int, doms: Array[Domain]): Array[IntDomain] = {
    supp.toArray
  }

  def supp: List[IntDomain]

}

object BDDM0 extends BDDM {
  id = 0
  def iterator = Iterator.empty
  def +(e: List[Int])(implicit cache: Cache): BDDM = e match {
    case Nil    => BDDMLeaf
    case h :: t => new BDDMNode(h, BDDM0 + t, BDDM0)
  }
  def lambda(map: IdMap[BDD, BigInt]) = 0
  def contains(e: Seq[Int]) = false
  def reduce(cache: collection.mutable.Map[BDDM, BDDM]) = BDDM0
  def edges(ts: Int) = 0

  def filterTrie(ts: Int, doms: Array[Domain], modified: List[Int], depth: Int, cache: Cache): BDDM =
    this
  def passTrie(ts: Int, doms: Array[Domain], modified: List[Int], depth: Int, cache: Cache): BDDM =
    this
  def filterModifiedTrie(ts: Int, doms: Array[Domain], modified: List[Int], depth: Int, cache: Cache): BDDM =
    this

  def identify(i: Int) = id

  def nodes(map: IdMap[BDD, Unit]) = map

  def supp = Nil
}

object BDDMLeaf extends BDDM {
  id = 1
  def iterator = Iterator(Seq())
  def +(e: List[Int])(implicit cache: Cache): BDDM = {
    require(e.isEmpty)
    this
  }
  def lambda(map: IdMap[BDD, BigInt]) = 1
  def contains(e: Seq[Int]) = true
  def reduce(cache: collection.mutable.Map[BDDM, BDDM]) = BDDMLeaf
  def edges(ts: Int) = 0
  def filterTrie(ts: Int, doms: Array[Domain], modified: List[Int], depth: Int, cache: Cache): BDDM = {
    this
  }
  def passTrie(ts: Int, doms: Array[Domain], modified: List[Int], depth: Int, cache: Cache): BDDM =
    this
  def filterModifiedTrie(ts: Int, doms: Array[Domain], modified: List[Int], depth: Int, cache: Cache): BDDM =
    this

  override def isEmpty = false
  def nodes(map: IdMap[BDD, Unit]) = map

  def supp = Nil
}

class BDDMNode(val index: Int, val child: BDDM, val sibling: BDDM) extends BDDM {
  assert(child.nonEmpty)

  private[extension] val cache: TSCache[BDDM] = new TSCache()

  def iterator = child.iterator.map(index +: _) ++ sibling.iterator

  def +(e: List[Int])(implicit cache: Cache): BDDM = {
    val h :: t = e
    if (index > h) {
      new BDDMNode(h, BDDM0 + t, this)
    } else if (index == h) {
      new BDDMNode(index, child + t, sibling)
    } else {
      new BDDMNode(index, child, sibling + e)
    }
  }

  def lambda(map: IdMap[BDD, BigInt]) =
    map.getOrElseUpdate(this, child.lambda(map) + sibling.lambda(map))

  def contains(e: Seq[Int]) = {
    val h +: t = e
    if (h > index) {
      false
    } else if (index == h) {
      child.contains(t)
    } else {
      sibling.contains(e)
    }
  }

  override def hashCode = ???

  override def equals(o: Any) = o match {
    case n: BDDMNode => (n.index == index) && (n.child eq child) && (n.sibling eq sibling)
    case _           => false
  }

  def edges(ts: Int) = {
    cache(ts, 0,
      1 + child.edges(ts) + sibling.edges(ts))
  }

  def filterTrie(ts: Int, doms: Array[Domain], modified: List[Int], depth: Int, nodeCache: Cache): BDDM = {
    if (modified.isEmpty) {
      this
    } else if (modified.head == depth) {
      filterModifiedTrie(ts, doms, modified.tail, depth, nodeCache)
    } else {
      passTrie(ts, doms, modified, depth, nodeCache)
    }

  }

  def passTrie(ts: Int, doms: Array[Domain], modified: List[Int], depth: Int, nodeCache: Cache): BDDM = {
    cache(ts) {
      val newChild = child.filterTrie(ts, doms, modified, depth + 1, nodeCache)
      val newSibling = sibling.passTrie(ts, doms, modified, depth, nodeCache)
      if (newChild.isEmpty) {
        newSibling
      } else if ((child eq newChild) && (sibling eq newSibling)) {
        this
      } else {
        new BDDMNode(index, newChild, newSibling)
      }
    }
  }

  def filterModifiedTrie(ts: Int, doms: Array[Domain], modified: List[Int], depth: Int, nodeCache: Cache): BDDM = {
    cache(ts) {
      val newSibling = sibling.filterModifiedTrie(ts, doms, modified, depth, nodeCache)
      if (doms(depth).present(index)) {
        val newChild = child.filterTrie(ts, doms, modified, depth + 1, nodeCache)
        if (newChild.isEmpty) {
          newSibling
        } else if ((child eq newChild) && (sibling eq newSibling)) {
          this
        } else {
          new BDDMNode(index, newChild, newSibling)
        }
      } else {
        newSibling
      }
    }
  }

  override def isEmpty = false

  def nodes(map: IdMap[BDD, Unit]) = {
    if (map.contains(this)) {
      map
    } else {
      sibling.nodes(child.nodes(map += ((this, ()))))
    }
  }

  lazy val supp: List[IntDomain] = {
    val others = sibling.supp
    if (others.isEmpty) {
      Singleton(index) :: child.supp
    } else {
      (others.head | index) :: merge(others.tail, child.supp)
    }
  }

  private def merge(d1: List[IntDomain], d2: List[IntDomain]): List[IntDomain] = {
    if (d1.isEmpty) d2
    else if (d2.isEmpty) d1
    else {
      val m = merge(d1.tail, d2.tail)

      (d1.head | d2.head).asInstanceOf[IntDomain] :: m
    }
  }

}