package concrete.constraint.extension

import scala.annotation.tailrec
import scala.util.hashing.MurmurHash3
import com.typesafe.scalalogging.LazyLogging
import concrete.priorityqueues.Identified
import concrete.util.SetWithMax
import cspom.extension.IdMap
import cspom.extension.IdSet
import concrete.Variable
import concrete.util.TSCache

object MDD {
  def apply(data: Seq[Array[Int]]): MDD = {
    data.foldLeft[MDD](MDD0)(
      (acc, tuple) => acc + tuple) //.reduce(new IdMap[Seq[MDD], MDD]())
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

}

trait MDD extends Identified with Iterable[Seq[Int]] with LazyLogging {

  val cache = new TSCache[MDD]()

  private var _id: Int = _
  def getId = _id

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

  def +(t: Array[Int]): MDD = if (contains(t)) MDD.this else addTrie(t, 0)
  def addTrie(t: Array[Int], i: Int): MDD
  def reduce(mdds: collection.mutable.Map[Seq[MDD], MDD]): MDD
  final def contains(t: Array[Int]): Boolean = contains(t, 0)
  def contains(t: Array[Int], i: Int): Boolean

  def findSupport(ts: Int, scope: Array[Variable], p: Int, i: Int, support: Array[Int], depth: Int): Option[Array[Int]]

  def checkSup(ts: Int, scope: Array[Variable], p: Int, i: Int, index: Int, next: MDD, support: Array[Int], depth: Int) = {
    val ok = if (p == depth) { i == index } else scope(depth).dom.present(index)
    if (ok) {
      support(depth) = index
      next.findSupport(ts, scope, p, i, support, depth + 1)
    } else {
      None
    }
  }

  def edges(ts: Int): Int

  def filterTrie(ts: Int, f: (Int, Int) => Boolean, modified: List[Int], depth: Int): MDD

  def fillFound(ts: Int, f: (Int, Int) => Boolean, depth: Int, l: SetWithMax)

  final def lambda: BigInt = {
    lambda(new IdMap())
  }

  final def lambda(map: IdMap[MDD, BigInt]): BigInt = {
    if (this eq MDDLeaf) {
      BigInt(1)
    } else {
      map.getOrElseUpdate(this, {
        var l = BigInt(0)
        forSubtries {
          case (_, m) =>
            l += m.lambda(map)
            true
        }
        l
      })
    }
  }

  def forSubtries(f: (Int, MDD) => Boolean): Boolean

  override def isEmpty: Boolean

  def copy(ts: Int): MDD

  override lazy val hashCode: Int = {
    logger.warn("Computed hashcode")
    MurmurHash3.unorderedHash(traverseST)
  }

  private def traverseST: Traversable[(Int, MDD)] = new Traversable[(Int, MDD)] {
    def foreach[A](f: ((Int, MDD)) => A) {
      forSubtries { (i, mdd) => f((i, mdd)); true }
    }
  }

  override def equals(o: Any): Boolean = o match {
    case MDDLeaf => false
    case t: MDD => t.traverseST.toIterable.zip(traverseST.toIterable).forall {
      case ((i1, s1), (i2, s2)) => i1 == i2 && (s1 eq s2)
    }
    case _ => false
  }

  def universal(scope: Array[Variable], timestamp: Int, position: Int = 0): Boolean = {
    (this ne MDD0) && ((this eq MDDLeaf) || cache(timestamp, true, {
      scope(position).dom.indices.forall { i =>
        subMDD(i).universal(scope, timestamp, position + 1)
      }
    }))
  }

  def subMDD(i: Int): MDD

}

final object MDDLeaf extends MDD {
  override def getId = 0
  //override def size = 1

  def reduce(mdds: collection.mutable.Map[Seq[MDD], MDD]) = this
  def contains(tuple: Array[Int], i: Int) = true
  override lazy val hashCode = 0
  def iterator = Iterator(Nil)
  def edges(ts: Int) = 0
  def filterTrie(ts: Int, f: (Int, Int) => Boolean, modified: List[Int], depth: Int) = {
    assert(modified.isEmpty)
    this
  }
  def fillFound(ts: Int, f: (Int, Int) => Boolean, depth: Int, l: SetWithMax) = {
    //assert(depth > l.max)
    //println("leaf at depth " + depth)
    l.clearFrom(depth)
  }
  def addTrie(tuple: Array[Int], i: Int) = throw new UnsupportedOperationException
  override def toString = "MDD Leaf"
  def forSubtries(f: (Int, MDD) => Boolean): Boolean = {
    throw new UnsupportedOperationException
  }
  override def size = 1
  override def isEmpty = false
  def findSupport(ts: Int, scope: Array[Variable], p: Int, i: Int, support: Array[Int], depth: Int) =
    Some(support)

  override def equals(o: Any) = o match {
    case r: AnyRef => r eq MDDLeaf
    case _ => false
  }

  def copy(ts: Int) = this

  def subMDD(i: Int) = MDDLeaf
}

final object MDD0 extends MDD {
  override def getId = throw new UnsupportedOperationException

  def reduce(mdds: collection.mutable.Map[Seq[MDD], MDD]) = MDD0
  def contains(tuple: Array[Int], i: Int) = false
  def iterator = Iterator()
  def edges(ts: Int) = 0
  def filterTrie(ts: Int, f: (Int, Int) => Boolean, modified: List[Int], depth: Int) = MDD0
  def fillFound(ts: Int, f: (Int, Int) => Boolean, depth: Int, l: SetWithMax) {
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

  def findSupport(ts: Int, scope: Array[Variable], p: Int, i: Int, support: Array[Int], depth: Int) =
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
        new MDD1(child.addTrie(t, i + 1), index)
      } else {
        new MDD2(child, index, MDD0.addTrie(t, i + 1), v)
      }
    }

  def contains(t: Array[Int], i: Int): Boolean = {
    t(i) == index && child.contains(t, i + 1)
  }

  def reduce(mdds: collection.mutable.Map[Seq[MDD], MDD]): MDD = {

    //    } else if (mdds.contains(trie)) {
    //      this
    //    } else

    var b = child.reduce(mdds)
    //
    //    if (b eq EmptyMDD) {
    //      EmptyMDD
    //    } else {
    val newArray = MDD.newTrie(index, b)
    mdds.getOrElseUpdate(newArray, new MDD1(b, index))
    //}

  }

  def findSupport(ts: Int, scope: Array[Variable], p: Int, i: Int, support: Array[Int], depth: Int) = {
    cache(ts, None,
      checkSup(ts, scope, p, i, index, child, support, depth))
  }

  def fillFound(ts: Int, f: (Int, Int) => Boolean, depth: Int, l: SetWithMax): Unit = {
    cache(ts, (), {
      if (depth <= l.max) {
        if (f(depth, index)) l -= depth
        child.fillFound(ts, f, depth + 1, l)
      }
    })
  }

  def filterTrie(ts: Int, f: (Int, Int) => Boolean, modified: List[Int], depth: Int = 0): MDD =
    if (modified.isEmpty) {
      this
    } else {
      cache(ts, {
        val nC =
          if (modified.head == depth) {
            // Some change at this level
            if (f(depth, index)) {
              child.filterTrie(ts, f, modified.tail, depth + 1)
            } else {
              MDD0
            }
          } else {
            // No change at this level (=> no need to call f())
            child.filterTrie(ts, f, modified, depth + 1)
          }

        if (nC eq MDD0) {
          MDD0
        } else if (nC eq child) {
          this
        } else {
          new MDD1(nC, index)
        }
      })

    }

  def iterator = child.iterator.map(index +: _)

  def edges(ts: Int): Int = cache(ts, 0, 1 + child.edges(ts))

  override def isEmpty = false

  def copy(ts: Int) = cache(ts, new MDD1(child.copy(ts), index))

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
        case `leftI` => new MDD2(left.addTrie(t, i + 1), leftI, right, rightI)
        case `rightI` => new MDD2(left, leftI, right.addTrie(t, i + 1), rightI)
        case v: Int =>
          val newArray = MDD.newTrie((leftI, left), (rightI, right), (v, MDD0.addTrie(t, i + 1)))
          new MDDn(newArray, Array(leftI, rightI, v), 3)

      }
    }

  def contains(t: Array[Int], i: Int): Boolean = t(i) match {
    case `leftI` => left.contains(t, i + 1)
    case `rightI` => right.contains(t, i + 1)
    case _ => false
  }

  def fillFound(ts: Int, f: (Int, Int) => Boolean, depth: Int, l: SetWithMax): Unit =
    cache(ts, (), {
      if (depth <= l.max) {
        if (f(depth, leftI)) l -= depth
        left.fillFound(ts, f, depth + 1, l)
        if (f(depth, rightI)) l -= depth
        right.fillFound(ts, f, depth + 1, l)
      }
    })

  @inline
  private def filteredTrie(ts: Int, f: (Int, Int) => Boolean, modified: List[Int], depth: Int, t: MDD, i: Int) = {
    if (f(depth, i)) {
      t.filterTrie(ts, f, modified, depth + 1)
    } else {
      MDD0
    }
  }

  def filterTrie(ts: Int, f: (Int, Int) => Boolean, modified: List[Int], depth: Int): MDD =
    if (modified.isEmpty) {
      this
    } else cache(ts, {
      var nL: MDD = null
      var nR: MDD = null

      if (modified.head == depth) {
        // Some change at this level
        nL = filteredTrie(ts, f, modified.tail, depth, left, leftI)
        nR = filteredTrie(ts, f, modified.tail, depth, right, rightI)
      } else {
        // No change at this level (=> no need to call f())
        nL = left.filterTrie(ts, f, modified, depth + 1)
        nR = right.filterTrie(ts, f, modified, depth + 1)
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

    })

  def iterator: Iterator[Seq[Int]] =
    left.iterator.map(leftI +: _) ++ right.iterator.map(rightI +: _)

  def edges(ts: Int): Int = cache(ts, 0, 2 + left.edges(ts) + right.edges(ts))

  def reduce(mdds: collection.mutable.Map[Seq[MDD], MDD]): MDD = {
    var bL = left.reduce(mdds)
    var bR = right.reduce(mdds)

    val nT = MDD.newTrie((leftI, bL), (rightI, bR))

    mdds.getOrElseUpdate(nT, new MDD2(bL, leftI, bR, rightI))
  }

  override def isEmpty = false

  def findSupport(ts: Int, scope: Array[Variable], p: Int, i: Int, support: Array[Int], depth: Int) =
    cache(ts, None,
      checkSup(ts, scope, p, i, leftI, left, support, depth).orElse(
        checkSup(ts, scope, p, i, rightI, right, support, depth)))

  def copy(ts: Int) = cache(ts, new MDD2(left.copy(ts), leftI, right.copy(ts), rightI))

  def subMDD(i: Int) = i match {
    case `leftI` => left
    case `rightI` => right
    case _ => MDD0
  }
}

final class MDDn(
  private val trie: Array[MDD],
  private val indices: Array[Int],
  private val nbIndices: Int) extends MDD {

  def copy(ts: Int) = cache(ts, new MDDn(trie map (t => if (t eq null) null else t.copy(ts)), indices.clone, nbIndices))

  def forSubtries(f: (Int, MDD) => Boolean) = forSubtries(f, nbIndices - 1)

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
        val ni = indices.padTo(nbIndices + 1, 0)
        ni(nbIndices) = v
        (ni, nbIndices + 1)
        newTrie(v) = MDD0.addTrie(tuple, i + 1)
        new MDDn(newTrie, ni, nbIndices + 1)
      } else {
        newTrie(v) = newTrie(v).addTrie(tuple, i + 1)
        new MDDn(newTrie, indices, nbIndices)
      }

    }
  }

  def reduce(mdds: collection.mutable.Map[Seq[MDD], MDD]): MDD = {

    var b = MDD.newTrie(indices.take(nbIndices).map(i => (i, trie(i).reduce(mdds))): _*)
    mdds.getOrElseUpdate(b, new MDDn(b, indices, nbIndices))
  }

  //@tailrec
  def contains(tuple: Array[Int], i: Int): Boolean = {
    val v = tuple(i)
    v < trie.length && (trie(v) ne null) && trie(v).contains(tuple, i + 1)
  }

  def fillFound(ts: Int, f: (Int, Int) => Boolean, depth: Int, l: SetWithMax): Unit = cache(ts, (), {
    var i = nbIndices - 1
    while (i >= 0 && depth <= l.max) {
      val ti = indices(i)
      if (f(depth, ti)) l -= depth
      trie(ti).fillFound(ts, f, depth + 1, l)
      i -= 1
    }
  })

  private def filteredTrie(ts: Int, f: (Int, Int) => Boolean, modified: List[Int], depth: Int): (Array[MDD], Int) = {

    val trie = this.trie
    val newTrie: Array[MDD] = new Array(trie.length)
    var ii = nbIndices - 1
    var nbNewIndices = nbIndices

    while (ii >= 0) {
      val i = indices(ii)
      if (f(depth, i)) {
        val uT = trie(i).filterTrie(ts, f, modified, depth + 1)
        if (uT eq MDD0) {
          nbNewIndices = remIndex(ii, nbNewIndices)
        } else {
          newTrie(i) = uT
        }
      } else {
        nbNewIndices = remIndex(ii, nbNewIndices)
      }
      ii -= 1
    }
    (newTrie, nbNewIndices)
  }

  private def remIndex(i: Int, nb: Int) = {
    val nbNewIndices = nb - 1
    val t = indices(nbNewIndices)
    indices(nbNewIndices) = indices(i)
    indices(i) = t
    nbNewIndices

  }

  private def passedTrie(ts: Int, f: (Int, Int) => Boolean, modified: List[Int], depth: Int): (Array[MDD], Int) = {
    val trie = this.trie
    val newTrie: Array[MDD] = new Array(trie.length)
    var ii = nbIndices - 1
    var nbNewIndices = nbIndices

    while (ii >= 0) {
      val i = indices(ii)
      val nT = trie(i).filterTrie(ts, f, modified, depth)
      if (nT eq MDD0) {
        nbNewIndices = remIndex(ii, nbNewIndices)
      } else {
        newTrie(i) = nT
      }
      ii -= 1
    }
    (newTrie, nbNewIndices)
  }

  def filterTrie(ts: Int, f: (Int, Int) => Boolean, modified: List[Int], depth: Int = 0): MDD =
    if (modified.isEmpty) {
      this
    } else cache(ts, {

      val (newTrie, nbNewIndices) =
        if (modified.head == depth) {
          // Some change at this level
          filteredTrie(ts, f, modified.tail, depth)
        } else {
          // No change at this level (=> no need to call f())
          passedTrie(ts, f, modified, depth + 1)
        }

      if (nbNewIndices == 0) {
        MDD0
      } else if (same(newTrie, nbNewIndices, trie)) {
        this
      } else {
        newNode(newTrie, nbNewIndices)
      }

    })

  private def newNode(t: Array[MDD], nbIndices: Int): MDD = {
    nbIndices match {
      case 1 => {
        val i = indices(0)
        new MDD1(t(i), i)
      }
      case 2 => {
        val i = indices(0)
        val j = indices(1)
        new MDD2(t(i), i, t(j), j)
      }
      case _ => new MDDn(t, indices, nbIndices)
    }

  }

  private def same(t1: Array[MDD], newSize: Int, t2: Array[MDD]): Boolean = {
    if (newSize != nbIndices) {
      false
    } else {
      var i = nbIndices - 1
      while (i >= 0 && (t1(indices(i)) eq t2(indices(i)))) {
        i -= 1
      }
      i < 0
    }
  }

  def iterator = indices.iterator.take(nbIndices).map(i => (trie(i), i)) flatMap {
    case (t, i) => t.iterator map (i +: _)
  }

  def edges(ts: Int): Int = cache(ts, 0,
    nbIndices + indices.take(nbIndices).map(trie(_)).map(_.edges(ts)).sum)

  override def isEmpty = false

  def findSupport(ts: Int, scope: Array[Variable], p: Int, i: Int, support: Array[Int], depth: Int) =
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
        var j = nbIndices - 1
        while (s.isEmpty && j >= 0) {
          val index = indices(j)
          if (scope(depth).dom.present(index)) {
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


