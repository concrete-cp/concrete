package concrete
package constraint.extension

import scala.collection.mutable.HashMap
import concrete.util.TSCache
import cspom.extension.IdMap
import concrete.util.SetWithMax
import scala.collection.mutable.ArrayBuffer

object MDDLink {
  def apply(t: Traversable[List[Int]]): MDDLink = {
    implicit def cache = null
    t.foldLeft[MDDLink](MDDLink0)(_ + _)
  }

  def apply(mdd: MDD): MDDLink = {

    val map: IdMap[MDD, MDDLink] = new IdMap()

    def mdd2mddLink(n: MDD): MDDLink = {
      if (n eq MDDLeaf) MDDLinkLeaf
      else {
        map.getOrElseUpdate(n, {
          n.traverseST.toSeq.sortBy(-_._1).foldLeft[MDDLink](MDDLink0) {
            case (acc, (i, m)) => new MDDLinkNode(i, mdd2mddLink(m), acc)
          }
        })
      }
    }

    mdd2mddLink(mdd)
  }

}

sealed trait MDDLink extends BDD {
  var id: Int = _

  override def +(e: List[Int])(implicit cache: Cache): MDDLink

  def reduce(): MDDLink = {

    val i = identify()

    val common = new Array[MDDLink](i + 1)
    common(0) = MDDLink0
    common(1) = MDDLinkLeaf

    def step2(n: MDDLink): MDDLink = {

      val idn = n.id
      if (common(idn) == null) {
        val nt = n.asInstanceOf[MDDLinkNode]
        common(idn) = new MDDLinkNode(nt.index, step2(nt.child), step2(nt.sibling))
      }
      common(idn)

    }

    step2(this)

  }

  def filterTrie(ts: Int, doms: Array[Domain], modified: List[Int], depth: Int, cache: Cache): MDDLink
  protected[extension] def passTrie(ts: Int, doms: Array[Domain], modified: List[Int], depth: Int, cache: Cache): MDDLink
  protected[extension] def filterModifiedTrie(ts: Int, doms: Array[Domain], modified: List[Int], depth: Int, cache: Cache): MDDLink

  def identify(): Int = {
    val cache = new HashMap[(Int, Int, Int), MDDLink]()

    def traverse(n: MDDLink, i: Int): Int = {
      n match {
        case nt: MDDLinkNode =>

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

}

object MDDLink0 extends MDDLink {
  id = 0
  def iterator = Iterator.empty
  def +(e: List[Int])(implicit cache: Cache): MDDLink = e match {
    case Nil    => MDDLinkLeaf
    case h :: t => new MDDLinkNode(h, MDDLink0 + t, MDDLink0)
  }
  def lambda(map: IdMap[BDD, BigInt]) = 0
  def contains(e: Seq[Int]) = false
  def reduce(cache: collection.mutable.Map[MDDLink, MDDLink]) = MDDLink0
  def edges(ts: Int) = 0

  def filterTrie(ts: Int, doms: Array[Domain], modified: List[Int], depth: Int, cache: Cache): MDDLink =
    this
  def passTrie(ts: Int, doms: Array[Domain], modified: List[Int], depth: Int, cache: Cache): MDDLink =
    this
  def filterModifiedTrie(ts: Int, doms: Array[Domain], modified: List[Int], depth: Int, cache: Cache): MDDLink =
    this

  def fillFound(ts: Int, f: (Int, Int) => Boolean, depth: Int, l: SetWithMax): Unit = ()
  def identify(i: Int) = id

  def nodes(map: IdMap[BDD, Unit]) = map
}

object MDDLinkLeaf extends MDDLink {
  id = 1
  def iterator = Iterator(Seq())
  def +(e: List[Int])(implicit cache: Cache): MDDLink = {
    require(e.isEmpty)
    this
  }
  def lambda(map: IdMap[BDD, BigInt]) = 1
  def contains(e: Seq[Int]) = true
  def reduce(cache: collection.mutable.Map[MDDLink, MDDLink]) = MDDLinkLeaf
  def edges(ts: Int) = 0
  def filterTrie(ts: Int, doms: Array[Domain], modified: List[Int], depth: Int, cache: Cache): MDDLink = {
    this
  }
  def passTrie(ts: Int, doms: Array[Domain], modified: List[Int], depth: Int, cache: Cache): MDDLink =
    this
  def filterModifiedTrie(ts: Int, doms: Array[Domain], modified: List[Int], depth: Int, cache: Cache): MDDLink =
    this
  def fillFound(ts: Int, f: (Int, Int) => Boolean, depth: Int, l: SetWithMax): Unit = {
    l.clearFrom(depth)
  }

  override def isEmpty = false
  def nodes(map: IdMap[BDD, Unit]) = map
}

class MDDLinkNode(val index: Int, val child: MDDLink, val sibling: MDDLink) extends MDDLink {
  assert(child.nonEmpty)

  private[extension] val cache: TSCache[MDDLink] = new TSCache()

  def iterator = child.iterator.map(index +: _) ++ sibling.iterator

  def +(e: List[Int])(implicit cache: Cache): MDDLink = {
    val h :: t = e
    if (index > h) {
      new MDDLinkNode(h, MDDLink0 + t, this)
    } else if (index == h) {
      new MDDLinkNode(index, child + t, sibling)
    } else {
      new MDDLinkNode(index, child, sibling + e)
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
    case n: MDDLinkNode => (n.index == index) && (n.child eq child) && (n.sibling eq sibling)
    case _              => false
  }

  def edges(ts: Int) = {
    cache(ts, 0,
      1 + child.edges(ts) + sibling.edges(ts))
  }

  def filterTrie(ts: Int, doms: Array[Domain], modified: List[Int], depth: Int, nodeCache: Cache): MDDLink = {
    if (modified.isEmpty) {
      this
    } else if (modified.head == depth) {
      filterModifiedTrie(ts, doms, modified.tail, depth, nodeCache)
    } else {
      passTrie(ts, doms, modified, depth, nodeCache)
    }

  }

  def passTrie(ts: Int, doms: Array[Domain], modified: List[Int], depth: Int, nodeCache: Cache): MDDLink = {
    cache(ts) {
      val newChild = child.filterTrie(ts, doms, modified, depth + 1, nodeCache)
      val newSibling = sibling.passTrie(ts, doms, modified, depth, nodeCache)
      if (newChild.isEmpty) {
        newSibling
      } else if ((child eq newChild) && (sibling eq newSibling)) {
        this
      } else {
        new MDDLinkNode(index, newChild, newSibling)
      }
    }
  }

  def filterModifiedTrie(ts: Int, doms: Array[Domain], modified: List[Int], depth: Int, nodeCache: Cache): MDDLink = {
    cache(ts) {
      val newSibling = sibling.filterModifiedTrie(ts, doms, modified, depth, nodeCache)
      if (doms(depth).present(index)) {
        val newChild = child.filterTrie(ts, doms, modified, depth + 1, nodeCache)
        if (newChild.isEmpty) {
          newSibling
        } else if ((child eq newChild) && (sibling eq newSibling)) {
          this
        } else {
          new MDDLinkNode(index, newChild, newSibling)
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