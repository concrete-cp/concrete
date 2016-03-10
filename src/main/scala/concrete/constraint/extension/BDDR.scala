package concrete
package constraint.extension

import java.lang.ref.ReferenceQueue
import java.lang.ref.WeakReference

import scala.Iterator
import scala.annotation.elidable.ASSERTION
import scala.collection.mutable.WeakHashMap
import scala.math.BigInt.int2bigInt
import scala.util.hashing.MurmurHash3

import concrete.Domain
import concrete.util.SetWithMax
import concrete.util.TSCache
import cspom.extension.IdMap

object BDDR {

  def apply(t: Traversable[List[Int]])(implicit cache: BDD.Cache): BDDR = t.foldLeft[BDDR](BDDR0)(_ + _)

  def apply(mdd: MDD, cache: BDD.Cache): BDDR = {
    val map: IdMap[MDD, BDDR] = new IdMap()

    def mdd2BDDR(n: MDD): BDDR = {
      if (n eq MDDLeaf) BDDRLeaf
      else {
        map.getOrElseUpdate(n, {
          n.traverseST.toSeq.sortBy(-_._1).foldLeft[BDDR](BDDR0) {
            case (acc, (i, m)) => BDDR(i, mdd2BDDR(m), acc, cache)
          }
        })
      }
    }

    mdd2BDDR(mdd)
  }

  // val cache = new WeakCache[BDDRNode](1000000)

  def apply(index: Int, child: BDDR, sibling: BDDR, cache: BDD.Cache) = {
    val n = new BDDRNode(index, child, sibling)

    val r = cache.getOrElseUpdate(n, new WeakReference(n)).get

    if (r eq null) {
      cache.put(n, new WeakReference(n))
      n
    } else {
      r
    }

    // n
  }
}

final class WeakCache[A >: Null <: AnyRef](proposedCapacity: Int) {

  private val buckets = {
    var capacity = 1
    while (capacity < proposedCapacity) {
      capacity <<= 1
    }
    capacity
  }

  private val contents = new Array[Entry](buckets)
  private val queue = new ReferenceQueue[AnyRef]

  private def bucket(o: AnyRef): Int = {
    var h = o.hashCode();

    // This function ensures that hashCodes that differ only by
    // constant multiples at each bit position have a bounded
    // number of collisions (approximately 8 at default load factor).
    h ^= (h >>> 20) ^ (h >>> 12);
    h ^= (h >>> 7) ^ (h >>> 4);

    h & (buckets - 1);
  }

  def cached(v: A): A = {
    clear()
    val i = bucket(v)
    val b = contents(i)
    val c = get(v, b)
    if (c ne null) {
      c
    } else {
      contents(i) = new Entry(v, b)
      v
    }
  }

  def size = {
    clear()
    var s = 0
    for (b <- contents) {
      var i = b
      while (i != null) {
        s += 1
        i = i.nextE
      }
    }
    s
  }

  @annotation.tailrec
  private def get(v: A, bucket: Entry): A = {
    if (bucket == null) {
      null
    } else {
      val e = bucket.get
      if (e == v) {
        e.asInstanceOf[A]
      } else {
        get(v, bucket.nextE)
      }
    }
  }

  @annotation.tailrec
  private def clear(): Unit = {
    val e = queue.poll.asInstanceOf[Entry]
    if (e ne null) {
      remove(e)
      clear()
    }
  }

  private def remove(r: Entry): Unit = {
    val first = bucket(r)
    var b = contents(first)
    if (b.get == r) {
      contents(first) = b.nextE
    } else {
      while (b.nextE ne null) {
        val next = b.nextE
        val e = next.get
        if (e == r) {
          b.nextE = next.nextE
          next.e = null
          return
        } else {
          b = next
        }
      }
    }

  }

  private class Entry(var e: A, var nextE: Entry) extends WeakReference[AnyRef](e, queue) {
    override val hashCode = e.hashCode
  }

}

sealed trait BDDR extends BDD {
  def +(e: List[Int])(implicit cache: Cache): BDDR

  def filterTrie(ts: Int, doms: Array[Domain], modified: List[Int], depth: Int, cache: Cache): BDDR
  protected[extension] def passTrie(ts: Int, doms: Array[Domain], modified: List[Int], depth: Int, cache: Cache): BDDR
  protected[extension] def filterModifiedTrie(ts: Int, doms: Array[Domain], modified: List[Int], depth: Int, cache: Cache): BDDR

 
  def reduce() = this

  def id = ???
  def identify() = ???
}

object BDDR0 extends BDDR {
  def iterator = Iterator.empty
  def +(e: List[Int])(implicit cache: Cache): BDDR = e match {
    case Nil    => BDDRLeaf
    case h :: t => BDDR(h, BDDR0 + t, BDDR0, cache)
  }
  def lambda(map: IdMap[BDD, BigInt]) = 0
  def contains(e: Seq[Int]) = false
  def reduce(cache: collection.mutable.Map[BDDR, BDDR]) = BDDR0
  def edges(ts: Int) = 0

  def filterTrie(ts: Int, doms: Array[Domain], modified: List[Int], depth: Int, cache: Cache): BDDR =
    this
  def passTrie(ts: Int, doms: Array[Domain], modified: List[Int], depth: Int, cache: Cache): BDDR =
    this
  def filterModifiedTrie(ts: Int, doms: Array[Domain], modified: List[Int], depth: Int, cache: Cache): BDDR =
    this

  def fillFound(ts: Int, f: (Int, Int) => Boolean, depth: Int, l: SetWithMax): Unit = ()

  def nodes(map: IdMap[BDD, Unit]) = map

  override def hashCode = 0

  override def equals(o: Any) = o match {
    case r: AnyRef => this eq r
    case _         => false
  }
}

object BDDRLeaf extends BDDR {
  def iterator = Iterator(Seq())
  def +(e: List[Int])(implicit cache: Cache): BDDR = {
    require(e.isEmpty)
    this
  }
  def lambda(map: IdMap[BDD, BigInt]) = 1
  def contains(e: Seq[Int]) = true
  def reduce(cache: collection.mutable.Map[BDDR, BDDR]) = BDDRLeaf
  def edges(ts: Int) = 0
  def filterTrie(ts: Int, doms: Array[Domain], modified: List[Int], depth: Int, cache: Cache): BDDR = {
    this
  }
  def passTrie(ts: Int, doms: Array[Domain], modified: List[Int], depth: Int, cache: Cache): BDDR =
    this
  def filterModifiedTrie(ts: Int, doms: Array[Domain], modified: List[Int], depth: Int, cache: Cache): BDDR =
    this
  def fillFound(ts: Int, f: (Int, Int) => Boolean, depth: Int, l: SetWithMax): Unit = {
    l.clearFrom(depth)
  }

  override def hashCode = 1

  override def isEmpty = false
  def nodes(map: IdMap[BDD, Unit]) = map

  override def equals(o: Any) = o match {
    case r: AnyRef => this eq r
    case _         => false
  }
}

case class BDDRNode(val index: Int, val child: BDDR, val sibling: BDDR) extends BDDR {
  assert(child.nonEmpty)

  private[extension] val cache: TSCache[BDDR] = new TSCache()

  def iterator = child.iterator.map(index +: _) ++ sibling.iterator

  def +(e: List[Int])(implicit cache: Cache): BDDR = {
    val h :: t = e
    if (index > h) {
      BDDR(h, BDDR0 + t, this, cache)
    } else if (index == h) {
      BDDR(index, child + t, sibling, cache)
    } else {
      BDDR(index, child, sibling + e, cache)
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

  override def equals(o: Any) = o match {
    case n: BDDRNode => (n.index == index) && (n.child eq child) && (n.sibling eq sibling)
    case _           => false
  }

  def edges(ts: Int) = {
    cache(ts, 0,
      1 + child.edges(ts) + sibling.edges(ts))
  }

  def filterTrie(ts: Int, doms: Array[Domain], modified: List[Int], depth: Int, cache: Cache): BDDR = {
    if (modified.isEmpty) {
      this
    } else if (modified.head == depth) {
      filterModifiedTrie(ts, doms, modified.tail, depth, cache)
    } else {
      passTrie(ts, doms, modified, depth, cache)
    }

  }

  def passTrie(ts: Int, doms: Array[Domain], modified: List[Int], depth: Int, nodeCache: Cache): BDDR = {
    cache(ts) {
      val newChild = child.filterTrie(ts, doms, modified, depth + 1, nodeCache)
      val newSibling = sibling.passTrie(ts, doms, modified, depth, nodeCache)
      if (newChild.isEmpty) {
        newSibling
      } else if ((child eq newChild) && (sibling eq newSibling)) {
        this
      } else {
        BDDR(index, newChild, newSibling, nodeCache)
      }
    }
  }

  def filterModifiedTrie(ts: Int, doms: Array[Domain], modified: List[Int], depth: Int, nodeCache: Cache): BDDR = {
    cache(ts) {
      val newSibling = sibling.filterModifiedTrie(ts, doms, modified, depth, nodeCache)
      if (doms(depth).present(index)) {
        val newChild = child.filterTrie(ts, doms, modified, depth + 1, nodeCache)
        if (newChild.isEmpty) {
          newSibling
        } else if ((child eq newChild) && (sibling eq newSibling)) {
          this
        } else {
          BDDR(index, newChild, newSibling, nodeCache)
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

  //  def _1 = index
  //  def _2 = child
  //  def _3 = sibling

  override val hashCode = MurmurHash3.productHash(this)

  def nodes(map: IdMap[BDD, Unit]) = {
    if (map.contains(this)) {
      map
    } else {
      sibling.nodes(child.nodes(map += ((this, ()))))
    }
  }

}