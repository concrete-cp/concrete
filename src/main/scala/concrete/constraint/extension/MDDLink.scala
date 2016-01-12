package concrete
package constraint.extension

import scala.collection.mutable.HashMap
import concrete.util.TSCache
import cspom.extension.IdMap
import concrete.util.SetWithMax
import scala.collection.mutable.ArrayBuffer

object MDDLink {
  def apply(t: Traversable[List[Int]]): MDDLink = t.foldLeft[MDDLink](MDDLink0)(_ + _)
}

sealed trait MDDLink extends Iterable[Seq[Int]] {
  var id: Int = _

  def +(e: List[Int]): MDDLink
  def lambda(map: IdMap[MDDLink, BigInt] = new IdMap()): BigInt
  def contains(e: Seq[Int]): Boolean
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
  def edges(ts: Int): Int

  def filterTrie(ts: Int, doms: Array[Domain], modified: List[Int], depth: Int): MDDLink
  protected[extension] def passTrie(ts: Int, doms: Array[Domain], modified: List[Int], depth: Int): MDDLink
  protected[extension] def filterModifiedTrie(ts: Int, doms: Array[Domain], modified: List[Int], depth: Int): MDDLink

  def fillFound(ts: Int, f: (Int, Int) => Boolean, depth: Int, l: SetWithMax): Unit

  def findSupport(ts: Int, scope: IndexedSeq[Domain], p: Int, i: Int, support: Array[Int], depth: Int): Option[Array[Int]] = {
    ???
  }

  def universal(scope: IndexedSeq[Domain], timestamp: Int): Boolean = ???

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
  def +(e: List[Int]): MDDLink = e match {
    case Nil    => MDDLinkLeaf
    case h :: t => new MDDLinkNode(h, MDDLink0 + t, MDDLink0)
  }
  def lambda(map: IdMap[MDDLink, BigInt]) = 0
  def contains(e: Seq[Int]) = false
  def reduce(cache: collection.mutable.Map[MDDLink, MDDLink]) = MDDLink0
  def edges(ts: Int) = 0

  def filterTrie(ts: Int, doms: Array[Domain], modified: List[Int], depth: Int): MDDLink =
    this
  def passTrie(ts: Int, doms: Array[Domain], modified: List[Int], depth: Int): MDDLink =
    this
  def filterModifiedTrie(ts: Int, doms: Array[Domain], modified: List[Int], depth: Int): MDDLink =
    this

  def fillFound(ts: Int, f: (Int, Int) => Boolean, depth: Int, l: SetWithMax): Unit = ()
  def identify(i: Int) = id

}

object MDDLinkLeaf extends MDDLink {
  id = 1
  def iterator = Iterator(Seq())
  def +(e: List[Int]): MDDLink = {
    require(e.isEmpty)
    this
  }
  def lambda(map: IdMap[MDDLink, BigInt]) = 1
  def contains(e: Seq[Int]) = true
  def reduce(cache: collection.mutable.Map[MDDLink, MDDLink]) = MDDLinkLeaf
  def edges(ts: Int) = 0
  def filterTrie(ts: Int, doms: Array[Domain], modified: List[Int], depth: Int): MDDLink = {
    this
  }
  def passTrie(ts: Int, doms: Array[Domain], modified: List[Int], depth: Int): MDDLink =
    this
  def filterModifiedTrie(ts: Int, doms: Array[Domain], modified: List[Int], depth: Int): MDDLink =
    this
  def fillFound(ts: Int, f: (Int, Int) => Boolean, depth: Int, l: SetWithMax): Unit = {
    l.clearFrom(depth)
  }

  override def isEmpty = false

}

class MDDLinkNode(val index: Int, val child: MDDLink, val sibling: MDDLink) extends MDDLink {
  assert(child.nonEmpty)

  private[extension] val cache: TSCache[MDDLink] = new TSCache()

  def iterator = child.iterator.map(index +: _) ++ sibling.iterator

  def +(e: List[Int]): MDDLink = {
    val h :: t = e
    if (index > h) {
      new MDDLinkNode(h, MDDLink0 + t, this)
    } else if (index == h) {
      new MDDLinkNode(index, child + t, sibling)
    } else {
      new MDDLinkNode(index, child, sibling + e)
    }
  }

  def lambda(map: IdMap[MDDLink, BigInt]) =
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

  override lazy val hashCode = {
    (index, child, sibling).hashCode
  }

  override def equals(o: Any) = o match {
    case n: MDDLinkNode => (n.index == index) && (n.child eq child) && (n.sibling eq sibling)
    case _              => false
  }

  def edges(ts: Int) = {
    cache(ts, 0,
      1 + child.edges(ts) + sibling.edges(ts))
  }

  def filterTrie(ts: Int, doms: Array[Domain], modified: List[Int], depth: Int): MDDLink = {
    if (modified.isEmpty) {
      this
    } else if (modified.head == depth) {
      filterModifiedTrie(ts, doms, modified.tail, depth)
    } else {
      passTrie(ts, doms, modified, depth)
    }

  }

  def passTrie(ts: Int, doms: Array[Domain], modified: List[Int], depth: Int): MDDLink = {
    cache(ts) {
      val newChild = child.filterTrie(ts, doms, modified, depth + 1)
      val newSibling = sibling.passTrie(ts, doms, modified, depth)
      if (newChild.isEmpty) {
        newSibling
      } else {
        new MDDLinkNode(index, newChild, newSibling)
      }
    }
  }

  def filterModifiedTrie(ts: Int, doms: Array[Domain], modified: List[Int], depth: Int): MDDLink = {
    cache(ts) {
      val newSibling = sibling.filterModifiedTrie(ts, doms, modified, depth)
      if (doms(depth).present(index)) {
        val newChild = child.filterTrie(ts, doms, modified, depth + 1)
        if (newChild.isEmpty) {
          newSibling
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

}