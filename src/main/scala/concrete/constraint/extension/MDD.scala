package concrete.constraint.extension

import scala.annotation.tailrec
import scala.util.hashing.MurmurHash3

import com.typesafe.scalalogging.slf4j.LazyLogging

import concrete.priorityqueues.Identified
import concrete.util.SetWithMax
import cspom.extension.IdMap
import cspom.extension.IdSet

final class Timestamp {
  var timestamp = 0
  def nextTimestamp(): Int = {
    timestamp += 1
    timestamp
  }
}

trait RelationGenerator {
  def apply(data: Iterator[Array[Int]]): Relation
}

object MDD extends RelationGenerator {
  def apply(data: Array[Int]*): MDD = apply(data.iterator)

  def apply(data: Iterator[Array[Int]]): MDD = {
    data.foldLeft[MDD](MDD0)(
      (acc, tuple) => acc + tuple).reduce(new IdMap[Seq[MDD], MDD]())
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

trait MDD extends Relation with Identified with LazyLogging {
  type Self2 = MDD

  private var mddTimestamp: Timestamp = null
  var timestamp: Int = 0

  def nextTimestamp() = {
    if (mddTimestamp == null) {
      registerTimestamp(new Timestamp(), new IdSet())
    }

    mddTimestamp.nextTimestamp()
  }

  private def registerTimestamp(ts: Timestamp, cache: IdSet[MDD]): Timestamp = {
    if (!cache(this) && (this ne MDDLeaf)) {
      mddTimestamp = ts
      forSubtries {
        (_, m: MDD) => m.registerTimestamp(ts, cache); true
      }
    }
    ts
  }

  var _id: Int = _
  def getId = _id
  def identify(): Int = {
    identify(nextTimestamp(), 1)
  }
  def identify(ts: Int, i: Int): Int = {
    if (ts == timestamp) {
      i
    } else {
      timestamp = ts
      var id = i
      _id = id
      forSubtries {
        (_, t) =>
          id = t.identify(ts, id + 1)
          true
      }
      id
    }
  }

  def +(t: Array[Int]): MDD = if (contains(t)) MDD.this else addTrie(t, 0)
  def addTrie(t: Array[Int], i: Int): MDD
  def reduce(mdds: collection.mutable.Map[Seq[MDD], MDD]): MDD
  def contains(t: Array[Int]): Boolean = contains(t, 0)
  def contains(t: Array[Int], i: Int): Boolean
  def find(f: (Int, Int) => Boolean): Option[Array[Int]] = {
    find(nextTimestamp(), f, 0) map (_.toArray)
  }
  def find(ts: Int, f: (Int, Int) => Boolean, depth: Int): Option[List[Int]]

  def findSupport(f: (Int, Int) => Boolean, p: Int, i: Int, support: Array[Int]): Option[Array[Int]] = {
    findSupport(nextTimestamp(), f, p, i, support, 0)
  }

  def findSupport(ts: Int, f: (Int, Int) => Boolean, p: Int, i: Int, support: Array[Int], depth: Int): Option[Array[Int]]

  def checkSupDepth(ts: Int, f: (Int, Int) => Boolean, p: Int, i: Int, index: Int, next: MDD, support: Array[Int]) = {
    if (i == index) {
      support(p) = i
      next.findSupport(ts, f, p, i, support, p + 1)
    } else {
      None
    }
  }

  def checkSupF(ts: Int, f: (Int, Int) => Boolean, p: Int, i: Int, index: Int, next: MDD, support: Array[Int], depth: Int) = {
    if (f(depth, index)) {
      support(depth) = index
      next.findSupport(ts, f, p, i, support, depth + 1)
    } else {
      None
    }
  }

  def iterator = listIterator.map(_.toArray)
  def listIterator: Iterator[List[Int]]
  def edges: Int = {
    edges(nextTimestamp())
  }
  def edges(ts: Int): Int
  def filterTrie(f: (Int, Int) => Boolean, modified: List[Int]): MDD = {
    filterTrie(nextTimestamp(), f, modified, 0)
  }
  def filterTrie(ts: Int, f: (Int, Int) => Boolean, modified: List[Int], depth: Int): MDD
  def fillFound(f: (Int, Int) => Boolean, arity: Int): Traversable[Int] = {
    val l = new SetWithMax(arity)
    fillFound(nextTimestamp(), f, 0, l)
    l
  }
  def fillFound(ts: Int, f: (Int, Int) => Boolean, depth: Int, l: SetWithMax)

  override def toString = s"$edges edges representing $size tuples"

  def -(t: Array[Int]) = throw new UnsupportedOperationException

  def lambda: BigInt = {
    lambda(new IdMap(), this)
  }

  def lambda(map: IdMap[MDD, BigInt], mdd: MDD): BigInt = {
    if (this eq MDDLeaf) {
      BigInt(1)
    } else {
      map.getOrElseUpdate(this, {
        var l = BigInt(0)
        forSubtries {
          case (_, m) =>
            l += m.lambda(map, m)
            true
        }
        l
      })
    }
  }

  def forSubtries(f: (Int, MDD) => Boolean)

  override def isEmpty: Boolean

  def copy = {
    copy(nextTimestamp())
  }

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

}

final object MDDLeaf extends MDD {
  override def getId = 0
  //override def size = 1
  def reduce(mdds: collection.mutable.Map[Seq[MDD], MDD]) = this
  def contains(tuple: Array[Int], i: Int) = true
  def find(ts: Int, f: (Int, Int) => Boolean, depth: Int) = Some(Nil)
  override lazy val hashCode = 0
  def listIterator = Iterator(Nil)
  def edges(ts: Int) = 0
  def filterTrie(ts: Int, f: (Int, Int) => Boolean, modified: List[Int], depth: Int) = {
    assert(modified.isEmpty)
    this
  }
  def fillFound(ts: Int, f: (Int, Int) => Boolean, depth: Int, l: SetWithMax) = {
    assert(depth > l.max)
  }
  def addTrie(tuple: Array[Int], i: Int) = throw new UnsupportedOperationException
  override def toString = "MDD Leaf"
  def forSubtries(f: (Int, MDD) => Boolean) {
    throw new UnsupportedOperationException
  }
  override def identify(ts: Int, i: Int): Int = i
  override def size = 1
  override def isEmpty = false
  def findSupport(ts: Int, f: (Int, Int) => Boolean, p: Int, i: Int, support: Array[Int], depth: Int) =
    Some(support)

  override def equals(o: Any) = o match {
    case r: AnyRef => r eq MDDLeaf
    case _ => false
  }

  def copy(ts: Int) = this
}

final object MDD0 extends MDD {
  override def getId = throw new UnsupportedOperationException
  override def identify(ts: Int, i: Int): Int = throw new UnsupportedOperationException
  def reduce(mdds: collection.mutable.Map[Seq[MDD], MDD]) = MDD0
  def contains(tuple: Array[Int], i: Int) = false
  def find(ts: Int, f: (Int, Int) => Boolean, depth: Int) = None
  def listIterator = Iterator()
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

  def forSubtries(f: (Int, MDD) => Boolean) {}

  override def isEmpty = true

  def findSupport(ts: Int, f: (Int, Int) => Boolean, p: Int, i: Int, support: Array[Int], depth: Int) =
    None

  def copy(ts: Int) = this
}

final class MDD1(private val child: MDD, private val index: Int) extends MDD {
  assert(child ne MDD0)

  def forSubtries(f: (Int, MDD) => Boolean) {
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

  def find(ts: Int, f: (Int, Int) => Boolean, depth: Int): Option[List[Int]] = {
    if (timestamp == ts) {
      None
    } else {
      timestamp = ts
      if (f(depth, index)) {
        child.find(ts, f, depth + 1).map(index :: _)
      } else {
        None
      }
    }
  }

  def findSupport(ts: Int, f: (Int, Int) => Boolean, p: Int, i: Int, support: Array[Int], depth: Int) = {
    if (timestamp == ts) {
      None
    } else {
      timestamp = ts
      if (depth == p) {
        checkSupDepth(ts, f, p, i, index, child, support)
      } else {
        checkSupF(ts, f, p, i, index, child, support, depth)
      }
    }
  }

  def fillFound(ts: Int, f: (Int, Int) => Boolean, depth: Int, l: SetWithMax) {
    if (timestamp != ts) {
      timestamp = ts
      if (depth <= l.max) {
        if (f(depth, index)) l -= depth
        if (depth + 1 <= l.max) {
          child.fillFound(ts, f, depth + 1, l)
        }
      }
    }
  }

  private var filteredResult: MDD = _

  def filterTrie(ts: Int, f: (Int, Int) => Boolean, modified: List[Int], depth: Int = 0): MDD =
    if (modified.isEmpty) {
      this
    } else if (ts == timestamp) {
      filteredResult
    } else {
      timestamp = ts

      //l nC =
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
        filteredResult = MDD0
      } else if (nC eq child) {
        filteredResult = this
      } else {
        filteredResult = new MDD1(nC, index)
      }

      filteredResult

    }

  def listIterator = child.listIterator.map(index :: _)

  def edges(ts: Int): Int = {
    if (ts == timestamp) {
      0
    } else {
      timestamp = ts
      1 + child.edges(ts)
    }
  }

  override def isEmpty = false
  def copy(ts: Int) =
    if (ts == timestamp) {
      filteredResult
    } else {
      timestamp = ts
      filteredResult = new MDD1(child.copy(ts), index)
      filteredResult
    }

}

final class MDD2(
  private val left: MDD, private val leftI: Int,
  private val right: MDD, private val rightI: Int) extends MDD {
  assert(right ne MDD0)
  assert(left ne MDD0)
  assert(leftI < rightI)

  def forSubtries(f: (Int, MDD) => Boolean) {
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

  def fillFound(ts: Int, f: (Int, Int) => Boolean, depth: Int, l: SetWithMax) {
    if (timestamp != ts) {
      timestamp = ts
      if (depth <= l.max) {
        if (f(depth, leftI)) l -= depth
        if (depth + 1 <= l.max) {
          left.fillFound(ts, f, depth + 1, l)
        }
        if (depth <= l.max) {
          if (f(depth, rightI)) l -= depth
          if (depth + 1 <= l.max) {
            right.fillFound(ts, f, depth + 1, l)
          }
        }
      }
    }
  }

  private var filteredResult: MDD = _

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
    } else if (ts == timestamp) {
      filteredResult
    } else {
      timestamp = ts

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

      filteredResult =
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

      filteredResult

    }

  def find(ts: Int, f: (Int, Int) => Boolean, depth: Int): Option[List[Int]] = {
    if (timestamp == ts) {
      None
    } else {
      timestamp = ts
      check(ts, f, depth, leftI, left).orElse(check(ts, f, depth, rightI, right))
    }
  }

  private def check(ts: Int, f: (Int, Int) => Boolean, depth: Int, direction: Int, trie: MDD): Option[List[Int]] = {
    if (f(depth, direction)) {
      trie.find(ts, f, depth + 1).map(direction :: _)
    } else {
      None
    }
  }

  def listIterator: Iterator[List[Int]] =
    left.listIterator.map(leftI :: _) ++ right.listIterator.map(rightI :: _)

  def edges(ts: Int): Int =
    if (ts == timestamp) {
      0
    } else {
      timestamp = ts
      2 + left.edges(ts) + right.edges(ts)
    }

  def reduce(mdds: collection.mutable.Map[Seq[MDD], MDD]): MDD = {
    var bL = left.reduce(mdds)
    var bR = right.reduce(mdds)

    val nT = MDD.newTrie((leftI, bL), (rightI, bR))

    mdds.getOrElseUpdate(nT, new MDD2(bL, leftI, bR, rightI))
  }

  override def isEmpty = false

  def findSupport(ts: Int, f: (Int, Int) => Boolean, p: Int, i: Int, support: Array[Int], depth: Int) = {
    if (timestamp == ts) {
      None
    } else {
      timestamp = ts
      if (depth == p) {
        checkSupDepth(ts, f, p, i, leftI, left, support).orElse(
          checkSupDepth(ts, f, p, i, rightI, right, support))
      } else {
        checkSupF(ts, f, p, i, leftI, left, support, depth).orElse(
          checkSupF(ts, f, p, i, rightI, right, support, depth))
      }
    }
  }

  def copy(ts: Int) =
    if (ts == timestamp) {
      filteredResult
    } else {
      timestamp = ts
      filteredResult = new MDD2(left.copy(ts), leftI, right.copy(ts), rightI)
      filteredResult
    }
}

final class MDDn(
  private val trie: Array[MDD],
  private val indices: Array[Int],
  private val nbIndices: Int) extends MDD {

  def copy(ts: Int) = {
    if (ts == timestamp) {
      filteredResult
    } else {
      timestamp = ts
      filteredResult = new MDDn(trie map (t => if (t eq null) null else t.copy(ts)), indices.clone, nbIndices)
      filteredResult
    }
  }

  def forSubtries(f: (Int, MDD) => Boolean) {
    forSubtries(f, nbIndices - 1)
  }

  @tailrec
  private def forSubtries(f: (Int, MDD) => Boolean, i: Int) {
    if (i >= 0) {
      val ti = indices(i)
      if (f(ti, trie(ti))) {
        forSubtries(f, i - 1)
      }
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

  def find(ts: Int, f: (Int, Int) => Boolean, depth: Int): Option[List[Int]] = {
    if (timestamp == ts) {
      None
    } else {
      timestamp = ts
      findHere(ts, f, depth)
    }
  }

  @tailrec
  private def findHere(ts: Int, f: (Int, Int) => Boolean, depth: Int, i: Int = nbIndices - 1): Option[List[Int]] = {
    if (i < 0) {
      None
    } else {
      val v = indices(i)
      if (f(depth, v)) {
        trie(v).find(ts, f, depth + 1) match {
          case Some(found) => Some(v :: found)
          case None => findHere(ts, f, depth, i - 1)
        }
      } else {
        findHere(ts, f, depth, i - 1)
      }
    }
  }

  def fillFound(ts: Int, f: (Int, Int) => Boolean, depth: Int, l: SetWithMax) {
    if (timestamp != ts) {
      timestamp = ts
      var i = nbIndices - 1
      while (i >= 0 && depth <= l.max) {
        val ti = indices(i)
        if (f(depth, ti)) l -= depth
        if (depth + 1 <= l.max) {
          trie(ti).fillFound(ts, f, depth + 1, l)
        }
        i -= 1
      }
    }
  }

  private var filteredResult: MDD = _

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
    } else if (ts == timestamp) {
      filteredResult
    } else {
      timestamp = ts

      val (newTrie, nbNewIndices) =
        if (modified.head == depth) {
          // Some change at this level
          filteredTrie(ts, f, modified.tail, depth)
        } else {
          // No change at this level (=> no need to call f())
          passedTrie(ts, f, modified, depth + 1)
        }

      filteredResult =
        if (nbNewIndices == 0) {
          MDD0
        } else if (same(newTrie, nbNewIndices, trie)) {
          this
        } else {
          newNode(newTrie, nbNewIndices)
        }
      filteredResult

    }

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

  def listIterator = indices.iterator.take(nbIndices).map(i => (trie(i), i)) flatMap {
    case (t, i) => t.listIterator map (i :: _)
  }

  def edges(ts: Int): Int = {
    if (ts == timestamp) {
      0
    } else {
      timestamp = ts
      nbIndices + indices.take(nbIndices).map(trie(_)).map(_.edges(ts)).sum
    }
  }

  override def isEmpty = false

  def findSupport(ts: Int, f: (Int, Int) => Boolean, p: Int, i: Int, support: Array[Int], depth: Int) = {
    if (timestamp == ts) {
      None
    } else {
      timestamp = ts

      if (depth == p) {
        if (i >= trie.length || (trie(i) eq null)) {
          None
        } else {
          support(depth) = i
          trie(i).findSupport(ts, f, p, i, support, depth + 1)
        }
      } else {
        var s: Option[Array[Int]] = None
        var j = nbIndices - 1
        while (s.isEmpty && j >= 0) {
          val index = indices(j)
          support(depth) = index
          s = trie(index).findSupport(ts, f, p, i, support, depth + 1)
          j -= 1
        }
        s
      }
    }
  }
}


