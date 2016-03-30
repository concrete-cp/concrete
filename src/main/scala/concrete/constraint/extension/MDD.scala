package concrete.constraint.extension

import scala.annotation.tailrec
import scala.util.hashing.MurmurHash3
import com.typesafe.scalalogging.LazyLogging
import concrete.priorityqueues.Identified
import concrete.util.SetWithMax
import cspom.extension.IdMap
import cspom.extension.IdSet
import concrete.util.TSCache
import concrete.Domain
import scala.collection.mutable.HashMap
import concrete.IntDomain
import concrete.util.SparseSeq

object MDD {
  def apply(data: Traversable[Seq[Int]]): MDD = {
    data.foldLeft[MDD](MDD0)(
      (acc, tuple) => acc + tuple.toArray) //.reduce(new IdMap[Seq[MDD], MDD]())
  }

  def newTrie(i: Int, v: MDD) = {
    val t = new Array[MDD](i + 1) //Array.fill[MDD](i + 1)(MDD0)
    t(i) = v
    t
  }
  //
  def newTrie(t: (Int, MDD)*) = {
    val s: Int = t.map(_._1).max
    val trie = new Array[MDD](s + 1)
    for ((i, v) <- t) {
      trie(i) = v
    }
    trie
  }

  def apply(t: Map[Int, MDD]): MDD = {
    t.toSeq match {
      case Seq()                   => MDD0
      case Seq((index, child))     => new MDD1(child, index)
      case Seq((i1, c1), (i2, c2)) => new MDD2(c1, i1, c2, i2)
      case e =>
        val indices = SparseSeq(t.map(_._1).toSeq: _*)
        new MDDn(newTrie(e: _*), indices)
    }
  }

  def same(t1: Map[Int, MDD], t2: Map[Int, MDD]): Boolean = {
    //t1.hashCode == t2.hashCode &&
    t1.size == t2.size &&
      t1.forall {
        case (k1, v1) => t2.get(k1).exists(v1 eq _)
      }
  }

}

trait MDD extends Identified with Iterable[Seq[Int]] with LazyLogging {

  val cache = new TSCache[MDD]()

  private var _id: Int = _
  def id = _id

  def identify(ts: Int, i: Int = 1): Int = {
    if (this eq MDDLeaf) {
      i
    } else {
      cache(ts, i, {
        var id = i
        _id = id
        forSubtries {
          (_, t) =>
            id = t.identify(ts, id + 1)
            true
        }
        id
      })
    }
  }

  def +(t: Seq[Int]): MDD = {
    val ta = t.toArray
    if (contains(ta)) MDD.this else addTrie(ta, 0)
  }
  def addTrie(t: Array[Int], i: Int): MDD

  //  def reduce(mdds: collection.mutable.Map[Map[Int, MDD], MDD] = new HashMap()): MDD = {
  //    MDD.nodes += 1
  //    val current = traverseST.toMap
  //
  //    mdds.getOrElseUpdate(current, {
  //
  //      val reduced = current.map { case (i, j) => i -> j.reduce(mdds) }
  //
  //      if (same(current, reduced)) {
  //        this
  //      } else {
  //        mdds.getOrElseUpdate(reduced, MDD(reduced))
  //      }
  //
  //    })
  //  }

  def reduce(): MDD = {

    val cache = new HashMap[Map[Int, Int], MDD]()
    val id = new IdMap[MDD, Int]()

    id(MDD0) = 0
    id(MDDLeaf) = 1
    var i = 2

    def step1(n: MDD): Unit = n match {
      case MDD0 | MDDLeaf => ()
      case n if !id.contains(n) =>
        for ((_, c) <- n.traverseST) step1(c)

        val idc = n.traverseST
          .map { case (i, c) => i -> id(c) }
          .toMap

        cache.get(idc) match {
          case Some(m) => id(n) = id(m)
          case None    => id(n) = i; i += 1
        }

        cache.update(idc, n)
      case _ => ()
    }

    step1(this)

    val common = new Array[MDD](i + 1)
    common(0) = MDD0
    common(1) = MDDLeaf

    def step2(n: MDD): MDD = {

      val idn = id(n)
      if (common(idn) == null) {
        common(idn) = MDD(n.traverseST.toMap.mapValues(step2)) //new MDDLinkNode(nt.index, step2(nt.child), step2(nt.sibling))
      }
      common(idn)

    }

    step2(this)

  }

  final def contains(t: Array[Int]): Boolean = contains(t, 0)
  def contains(t: Array[Int], i: Int): Boolean

  def findSupport(ts: Int, scope: IndexedSeq[Domain], p: Int, i: Int, support: Array[Int], depth: Int): Option[Array[Int]]

  def checkSup(ts: Int, domains: IndexedSeq[Domain], p: Int, i: Int, index: Int, next: MDD, support: Array[Int], depth: Int) = {
    val ok = if (p == depth) { i == index } else domains(depth).present(index)
    if (ok) {
      support(depth) = index
      next.findSupport(ts, domains, p, i, support, depth + 1)
    } else {
      None
    }
  }

  def edges(ts: Int): Int

  def filterTrie(ts: Int, doms: Array[Domain], modified: List[Int], depth: Int): MDD

  def supported(ts: Int, doms: Array[Domain], newDomains: Array[IntDomain], depth: Int, l: SetWithMax): Unit

  final def lambda: BigInt = {
    lambda(new IdMap())
  }

  final def lambda(map: IdMap[MDD, BigInt]): BigInt = {
    if (this eq MDDLeaf) {
      BigInt(1)
    } else {
      map.getOrElseUpdate(this, traverseST.map { case (_, m) => m.lambda(map) }.sum)
    }
  }

  def forSubtries(f: (Int, MDD) => Boolean): Boolean

  override def isEmpty: Boolean

  def copy(ts: Int): MDD

  override lazy val hashCode: Int = {
    //logger.warn("Computed hashcode")
    MurmurHash3.unorderedHash(traverseST)
  }

  def traverseST: Traversable[(Int, MDD)] = new Traversable[(Int, MDD)] {
    def foreach[A](f: ((Int, MDD)) => A) {
      forSubtries { (i, mdd) => f((i, mdd)); true }
    }
  }

  override def equals(o: Any): Boolean = o match {
    case MDDLeaf => this eq MDDLeaf
    case t: MDD =>
      {
        val t1 = t.traverseST.toMap
        val t2 = traverseST.toMap

        MDD.same(t1, t2)
      }
    //
    //      t.traverseST.toIterable.zip(traverseST.toIterable).forall {
    //        case ((i1, s1), (i2, s2)) => i1 == i2 && (s1 eq s2)
    //      }
    case _ => false
  }

  def universal(scope: IndexedSeq[Domain], timestamp: Int, position: Int = 0): Boolean = {
    (this ne MDD0) && ((this eq MDDLeaf) || cache(timestamp, true, {
      scope(position).forall { i =>
        subMDD(i).universal(scope, timestamp, position + 1)
      }
    }))
  }

  def subMDD(i: Int): MDD

  override def toString = System.identityHashCode(this).toString

  def nodes(map: IdMap[MDD, Unit]): IdMap[MDD, Unit] = {
    if (map.contains(this)) {
      map
    } else {
      traverseST.foldLeft(map += ((this, ()))) {
        case (map, (_, MDDLeaf)) => map
        case (map, (_, n))       => n.nodes(map)
      }
    }
  }
}

final object MDDLeaf extends MDD {
  override def id = 0
  //override def size = 1

  //override def reduce(mdds: collection.mutable.Map[Map[Int, MDD], MDD]) = this
  def contains(tuple: Array[Int], i: Int) = true
  override lazy val hashCode = 0
  def iterator = Iterator(Nil)
  def edges(ts: Int) = 0
  def filterTrie(ts: Int, doms: Array[Domain], modified: List[Int], depth: Int) = {
    assert(modified.isEmpty)
    this
  }
  def supported(ts: Int, doms: Array[Domain], newDoms: Array[IntDomain], depth: Int, l: SetWithMax) = {
    //assert(depth > l.max)
    //println("leaf at depth " + depth)
    l.clearFrom(depth)
  }

  def addTrie(tuple: Array[Int], i: Int) = {
    require(i >= tuple.length)
    this //throw new UnsupportedOperationException
  }
  override def toString = "MDD Leaf"
  def forSubtries(f: (Int, MDD) => Boolean): Boolean = {
    throw new UnsupportedOperationException
  }
  override def size = 1
  override def isEmpty = false
  def findSupport(ts: Int, scope: IndexedSeq[Domain], p: Int, i: Int, support: Array[Int], depth: Int) =
    Some(support)

  override def equals(o: Any) = o match {
    case r: AnyRef => r eq MDDLeaf
    case _         => false
  }

  def copy(ts: Int) = this

  def subMDD(i: Int) = MDDLeaf

}

final object MDD0 extends MDD {
  override def id = throw new UnsupportedOperationException

  def reduce(mdds: collection.mutable.Map[Seq[MDD], MDD]) = MDD0
  def contains(tuple: Array[Int], i: Int) = false
  def iterator = Iterator()
  def edges(ts: Int) = 0
  def filterTrie(ts: Int, doms: Array[Domain], modified: List[Int], depth: Int) = MDD0
  def supported(ts: Int, doms: Array[Domain], nd: Array[IntDomain], depth: Int, l: SetWithMax) = {
    throw new UnsupportedOperationException
  }
  def addTrie(tuple: Array[Int], i: Int) = {
    if (i >= tuple.length) {
      MDDLeaf
    } else {
      new MDD1(MDD0.addTrie(tuple, i + 1), tuple(i))
    }
  }

  override def toString = "Empty MDD"

  def forSubtries(f: (Int, MDD) => Boolean) = true

  override def isEmpty = true

  def findSupport(ts: Int, scope: IndexedSeq[Domain], p: Int, i: Int, support: Array[Int], depth: Int) =
    None

  def copy(ts: Int) = this

  def subMDD(i: Int) = MDD0
}

final class MDD1(private val child: MDD, private val index: Int) extends MDD {
  assert(child ne MDD0)

  def forSubtries(f: (Int, MDD) => Boolean) = {
    f(index, child)
  }

  def addTrie(t: Array[Int], i: Int): MDD =
    if (i >= t.length) {
      MDDLeaf
    } else {
      val v = t(i)
      if (v == index) {

        val nc = child.addTrie(t, i + 1)
        if (nc eq child) {
          this
        } else {
          new MDD1(nc, v)
        }

      } else {
        new MDD2(child, index, MDD0.addTrie(t, i + 1), v)
      }
    }

  def contains(t: Array[Int], i: Int): Boolean = {
    t(i) == index && child.contains(t, i + 1)
  }

  //  def reduce(mdds: collection.mutable.Map[Seq[MDD], MDD]): MDD = {
  //    val b = child.reduce(mdds)
  //    val newArray = MDD.newTrie(index, b)
  //    mdds.getOrElseUpdate(newArray, new MDD1(b, index))
  //
  //  }

  def findSupport(ts: Int, scope: IndexedSeq[Domain], p: Int, i: Int, support: Array[Int], depth: Int) = {
    cache(ts, None,
      checkSup(ts, scope, p, i, index, child, support, depth))
  }

  def supported(ts: Int, doms: Array[Domain], newDomains: Array[IntDomain], depth: Int, l: SetWithMax): Unit = {
    cache(ts, (), {
      if (depth <= l.max) {
        newDomains(depth) |= index
        if (newDomains(depth).length == doms(depth).length) l -= depth
        child.supported(ts, doms, newDomains, depth + 1, l)
      }
    })
  }

  def filterTrie(ts: Int, doms: Array[Domain], modified: List[Int], depth: Int = 0): MDD =
    if (modified.isEmpty) {
      this
    } else {
      cache(ts) {
        val nC =
          if (modified.head == depth) {
            // Some change at this level
            if (doms(depth).present(index)) {
              child.filterTrie(ts, doms, modified.tail, depth + 1)
            } else {
              MDD0
            }
          } else {
            // No change at this level (=> no need to call f())
            child.filterTrie(ts, doms, modified, depth + 1)
          }

        if (nC eq MDD0) {
          MDD0
        } else if (nC eq child) {
          this
        } else {
          new MDD1(nC, index)
        }
      }

    }

  def iterator = child.iterator.map(index +: _)

  def edges(ts: Int): Int = cache(ts, 0, 1 + child.edges(ts))

  override def isEmpty = false

  def copy(ts: Int) = cache(ts)(new MDD1(child.copy(ts), index))

  def subMDD(i: Int) = if (index == i) child else MDD0
}

final class MDD2(
    private val left: MDD, private val leftI: Int,
    private val right: MDD, private val rightI: Int) extends MDD {
  assert(right ne MDD0)
  assert(left ne MDD0)

  def forSubtries(f: (Int, MDD) => Boolean) = {
    f(leftI, left) && f(rightI, right)
  }

  def addTrie(t: Array[Int], i: Int): MDD =
    if (i >= t.length) {
      MDDLeaf
    } else {
      t(i) match {
        case `leftI` =>
          val nl = left.addTrie(t, i + 1)
          if (nl eq left) {
            this
          } else {
            new MDD2(nl, leftI, right, rightI)
          }
        case `rightI` =>
          val nr = right.addTrie(t, i + 1)
          if (nr eq right) {
            this
          } else {
            new MDD2(left, leftI, nr, rightI)
          }
        case v: Int =>
          val newArray = MDD.newTrie((leftI, left), (rightI, right), (v, MDD0.addTrie(t, i + 1)))
          new MDDn(newArray, SparseSeq(leftI, rightI, v))

      }
    }

  def contains(t: Array[Int], i: Int): Boolean = t(i) match {
    case `leftI`  => left.contains(t, i + 1)
    case `rightI` => right.contains(t, i + 1)
    case _        => false
  }

  def supported(ts: Int, doms: Array[Domain], newDoms: Array[IntDomain], depth: Int, l: SetWithMax): Unit =
    cache(ts, (), {
      if (depth <= l.max) {
        newDoms(depth) |= leftI
        newDoms(depth) |= rightI
        if (newDoms(depth).length == doms(depth).length) l -= depth
        left.supported(ts, doms, newDoms, depth + 1, l)
        right.supported(ts, doms, newDoms, depth + 1, l)
      }
    })

  @inline
  private def filteredTrie(ts: Int, doms: Array[Domain], modified: List[Int], depth: Int, t: MDD, i: Int) = {
    if (doms(depth).present(i)) {
      t.filterTrie(ts, doms, modified, depth + 1)
    } else {
      MDD0
    }
  }

  def filterTrie(ts: Int, doms: Array[Domain], modified: List[Int], depth: Int): MDD =
    if (modified.isEmpty) {
      this
    } else cache(ts) {
      var nL: MDD = null
      var nR: MDD = null

      if (modified.head == depth) {
        // Some change at this level
        nL = filteredTrie(ts, doms, modified.tail, depth, left, leftI)
        nR = filteredTrie(ts, doms, modified.tail, depth, right, rightI)
      } else {
        // No change at this level (=> no need to check presence)
        nL = left.filterTrie(ts, doms, modified, depth + 1)
        nR = right.filterTrie(ts, doms, modified, depth + 1)
      }

      if (nL eq MDD0) {
        if (nR eq MDD0) {
          MDD0
        } else {
          new MDD1(nR, rightI)
        }
      } else if (nR eq MDD0) {
        new MDD1(nL, leftI)
      } else {
        if ((nL eq left) && (nR eq right)) {
          this
        } else {
          new MDD2(nL, leftI, nR, rightI)
        }
      }

    }

  def iterator: Iterator[Seq[Int]] =
    left.iterator.map(leftI +: _) ++ right.iterator.map(rightI +: _)

  def edges(ts: Int): Int = cache(ts, 0, 2 + left.edges(ts) + right.edges(ts))

  //  def reduce(mdds: collection.mutable.Map[Seq[MDD], MDD]): MDD = {
  //    val bL = left.reduce(mdds)
  //    val bR = right.reduce(mdds)
  //
  //    val nT = MDD.newTrie((leftI, bL), (rightI, bR))
  //
  //    mdds.getOrElseUpdate(nT, new MDD2(bL, leftI, bR, rightI))
  //  }

  override def isEmpty = false

  def findSupport(ts: Int, scope: IndexedSeq[Domain], p: Int, i: Int, support: Array[Int], depth: Int) =
    cache(ts, None,
      checkSup(ts, scope, p, i, leftI, left, support, depth).orElse(
        checkSup(ts, scope, p, i, rightI, right, support, depth)))

  def copy(ts: Int) = cache(ts)(new MDD2(left.copy(ts), leftI, right.copy(ts), rightI))

  def subMDD(i: Int) = i match {
    case `leftI`  => left
    case `rightI` => right
    case _        => MDD0
  }
}

final class MDDn(
    private val trie: Array[MDD],
    private val indices: SparseSeq[Int]) extends MDD {

  def copy(ts: Int) = cache(ts)(new MDDn(trie.map(t => if (t eq null) null else t.copy(ts)), indices.copy))

  def forSubtries(f: (Int, MDD) => Boolean) = forSubtries(f, indices.size - 1)

  @tailrec
  private def forSubtries(f: (Int, MDD) => Boolean, i: Int): Boolean = {
    if (i < 0) {
      true
    } else {
      val ti = indices(i)
      f(ti, trie(ti)) && forSubtries(f, i - 1)
    }
  }

  def addTrie(tuple: Array[Int], i: Int): MDD = {
    if (i >= tuple.length) {
      MDDLeaf
    } else {
      val v = tuple(i)
      val newTrie = trie.padTo(v + 1, null)

      if ((newTrie(v) eq null) || (newTrie(v) eq MDD0)) {
        newTrie(v) = MDD0.addTrie(tuple, i + 1)
        new MDDn(newTrie, indices + v)
      } else {
        val ntv = newTrie(v).addTrie(tuple, i + 1)

        if (ntv eq newTrie(v)) {
          this
        } else {
          newTrie(v) = ntv
          new MDDn(newTrie, indices)
        }
      }

    }
  }

  //  def reduce(mdds: collection.mutable.Map[Seq[MDD], MDD]): MDD = {
  //    val b = MDD.newTrie(indices.take(nbIndices).map(i => (i, trie(i).reduce(mdds))): _*)
  //    mdds.getOrElseUpdate(b, new MDDn(b, indices, nbIndices))
  //  }

  //@tailrec
  def contains(tuple: Array[Int], i: Int): Boolean = {
    val v = tuple(i)
    v < trie.length && (trie(v) ne null) && trie(v).contains(tuple, i + 1)
  }

  def supported(ts: Int, doms: Array[Domain], newDomains: Array[IntDomain], depth: Int, l: SetWithMax): Unit = cache(ts, (), {

    newDomains(depth) ++= indices

    if (newDomains(depth).length == doms(depth).length) l -= depth

    if (depth <= l.max) {
      for (ti <- indices) {
        trie(ti).supported(ts, doms, newDomains, depth + 1, l)
      }
    }
  })

  private def filteredTrie(ts: Int, doms: Array[Domain], modified: List[Int], depth: Int): (Array[MDD], SparseSeq[Int]) = {

    val trie = this.trie
    val newTrie: Array[MDD] = new Array(trie.length)

    val ind = indices.filter { i =>
      doms(depth).present(i) && {
        val uT = trie(i).filterTrie(ts, doms, modified, depth + 1)
        if (uT eq MDD0) {
          false
        } else {
          newTrie(i) = uT
          true
        }
      }
    }
    (newTrie, ind)
  }

  private def passedTrie(ts: Int, doms: Array[Domain], modified: List[Int], depth: Int): (Array[MDD], SparseSeq[Int]) = {
    val trie = this.trie
    val newTrie: Array[MDD] = new Array(trie.length)

    val ind = indices.filter { i =>
      val nT = trie(i).filterTrie(ts, doms, modified, depth)
      if (nT eq MDD0) {
        false
      } else {
        newTrie(i) = nT
        true
      }
    }
    (newTrie, ind)
  }

  def filterTrie(ts: Int, doms: Array[Domain], modified: List[Int], depth: Int = 0): MDD =
    if (modified.isEmpty) {
      this
    } else cache(ts) {

      val (newTrie, newIndices) =
        if (modified.head == depth) {
          // Some change at this level
          filteredTrie(ts, doms, modified.tail, depth)
        } else {
          // No change at this level (=> no need to call f())
          passedTrie(ts, doms, modified, depth + 1)
        }

      if (newIndices.isEmpty) {
        MDD0
      } else if (same(newTrie, newIndices.size, trie)) {
        this
      } else {
        newNode(newTrie, newIndices)
      }

    }

  private def newNode(t: Array[MDD], newIndices: SparseSeq[Int]): MDD = {
    newIndices.size match {
      case 1 => {
        val i = indices(0)
        new MDD1(t(i), i)
      }
      case 2 => {
        val i = indices(0)
        val j = indices(1)
        new MDD2(t(i), i, t(j), j)
      }
      case _ => new MDDn(t, newIndices)
    }

  }

  private def same(t1: Array[MDD], newSize: Int, t2: Array[MDD]): Boolean = {
    (newSize == indices.size) && {
      var i = newSize - 1
      while (i >= 0 && (t1(indices(i)) eq t2(indices(i)))) {
        i -= 1
      }
      i < 0
    }
  }

  def iterator = indices.iterator.map(i => (trie(i), i)) flatMap {
    case (t, i) => t.iterator map (i +: _)
  }

  def edges(ts: Int): Int = cache(ts, 0,
    indices.size + indices.map(trie(_)).map(_.edges(ts)).sum)

  override def isEmpty = false

  def findSupport(ts: Int, scope: IndexedSeq[Domain], p: Int, i: Int, support: Array[Int], depth: Int) =
    cache(ts, None, {

      if (depth == p) {
        if (i >= trie.length || (trie(i) eq null)) {
          None
        } else {
          support(depth) = i
          trie(i).findSupport(ts, scope, p, i, support, depth + 1)
        }
      } else {

        var s: Option[Array[Int]] = None
        var j = indices.size - 1
        while (s.isEmpty && j >= 0) {
          val index = indices(j)
          if (scope(depth).present(index)) {
            support(depth) = index
            s = trie(index).findSupport(ts, scope, p, i, support, depth + 1)
          }
          j -= 1
        }
        s
      }
    })

  def subMDD(i: Int) = {
    if (i >= trie.length || (trie(i) eq null)) {
      MDD0
    } else {
      trie(i)
    }
  }
}


