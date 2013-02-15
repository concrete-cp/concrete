package cspfj.constraint.extension

import scala.annotation.tailrec
import scala.collection.mutable.HashMap
import scala.collection.mutable.Seq
import scala.util.hashing.MurmurHash3
import cspfj.util.ListWithMax
import java.util.Arrays
import cspfj.priorityqueues.Identified
import cspfj.util.BitVector
import cspfj.Variable

trait RelationGenerator {
  def apply(data: Iterator[Array[Int]]): Relation
}

object MDD extends RelationGenerator {
  def apply(data: Array[Int]*): MDD = apply(data.iterator)

  def apply(data: Iterator[Array[Int]]): MDD = {
    data.foldLeft[MDD](MDD0)(
      (acc, tuple) => acc + tuple).reduce(new HashMap[Seq[MDD], MDD]())
  }

  var timestamp = 0

  def newNode(t: Array[MDD]): MDD = {
    var found1I, found2I, found3I: Int = -1
    var found1N, found2N, found3N: MDD = null

    var i = t.length - 1
    while (i >= 0) {
      if (t(i) ne MDD0) {
        if (found1I < 0) {
          found1I = i
          found1N = t(i)
        } else if (found2I < 0) {
          found2I = i
          found2N = t(i)
        } else if (found3I < 0) {
          found3I = i
          found3N = t(i)
        } else {
          return new MDDn(t)
        }
      }
      i -= 1
    }

    assert(found1I >= 0)
    if (found2I < 0) {
      new MDD1(found1N, found1I)
    } else if (found3I < 0) {
      new MDD2(found2N, found2I, found1N, found1I)
    } else {
      new MDD3(found3N, found3I, found2N, found2I, found1N, found1I)
    }
  }

  def newTrie(i: Int, v: MDD) = {
    val t = Array.fill[MDD](i + 1)(MDD0) //new Array[MDD](i + 1)
    t(i) = v
    t
  }

  def newTrie(t: (Int, MDD)*) = {
    val s: Int = t.map(_._1).max
    val trie = Array.fill[MDD](s + 1)(MDD0)
    addTrie(trie, t: _*)
    trie
  }

  def addTrie(trie: Array[MDD], t: (Int, MDD)*) {
    for ((i, v) <- t) {
      trie(i) = v
    }
  }

}

trait MDD extends Relation with Identified {
  type Self2 = MDD
  var _id: Int = _
  def getId = _id
  def identify(): Int = {
    MDD.timestamp += 1
    identify(MDD.timestamp, 1)
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
  def timestamp: Int
  def timestamp_=(i: Int)
  def +(t: Array[Int]): MDD = if (contains(t)) MDD.this else addTrie(t, 0)
  def addTrie(t: Array[Int], i: Int): MDD
  def reduce(mdds: collection.mutable.Map[Seq[MDD], MDD]): MDD
  def contains(t: Array[Int]): Boolean = contains(t, 0)
  def contains(t: Array[Int], i: Int): Boolean
  def find(f: (Int, Int) => Boolean): Option[Array[Int]] = {
    MDD.timestamp += 1
    find(MDD.timestamp, f, 0) map (_.toArray)
  }
  def find(ts: Int, f: (Int, Int) => Boolean, depth: Int): Option[List[Int]]
  def iterator = listIterator.map(_.toArray)
  def listIterator: Iterator[List[Int]]
  def nodes: Int = {
    MDD.timestamp += 1
    nodes(MDD.timestamp)
  }
  def nodes(ts: Int): Int
  def filterTrie(f: (Int, Int) => Boolean, modified: List[Int]): MDD = {
    MDD.timestamp += 1
    filterTrie(MDD.timestamp, f, modified, 0)
  }
  def filterTrie(ts: Int, f: (Int, Int) => Boolean, modified: List[Int], depth: Int): MDD
  def fillFound(f: (Int, Int) => Boolean, arity: Int): Traversable[Int] = {
    MDD.timestamp += 1
    val l = new ListWithMax(arity)
    fillFound(MDD.timestamp, f, 0, l)
    l
  }
  def fillFound(ts: Int, f: (Int, Int) => Boolean, depth: Int, l: ListWithMax)

  override def toString = s"$nodes nodes representing $size tuples"
  def copy = this
  def -(t: Array[Int]) = throw new UnsupportedOperationException

  def forSubtries(f: (Int, MDD) => Boolean)
  override def size = {
    var s = 0
    forSubtries {
      (_, t) =>
        s += t.size
        true
    }
    s
  }
  override def isEmpty: Boolean
}

final object MDDLeaf extends MDD {
  var timestamp = 0
  override def getId = 0
  //override def size = 1
  def reduce(mdds: collection.mutable.Map[Seq[MDD], MDD]) = this
  def contains(tuple: Array[Int], i: Int) = true
  def find(ts: Int, f: (Int, Int) => Boolean, depth: Int) = Some(Nil)
  def listIterator = Iterator(Nil)
  def nodes(ts: Int) = {
    if (ts == timestamp) {
      0
    } else {
      timestamp = ts
      1
    }
  }
  def filterTrie(ts: Int, f: (Int, Int) => Boolean, modified: List[Int], depth: Int) = {
    assert(modified.isEmpty)
    this
  }
  def fillFound(ts: Int, f: (Int, Int) => Boolean, depth: Int, l: ListWithMax) = {
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
}

final object MDD0 extends MDD {
  //override def size = 0
  override def getId = throw new UnsupportedOperationException
  override def identify(ts: Int, i: Int): Int = throw new UnsupportedOperationException
  def timestamp = throw new UnsupportedOperationException
  def timestamp_=(i: Int) { throw new UnsupportedOperationException }
  def reduce(mdds: collection.mutable.Map[Seq[MDD], MDD]) = MDD0
  def contains(tuple: Array[Int], i: Int) = false
  def find(ts: Int, f: (Int, Int) => Boolean, depth: Int) = None
  def listIterator = Iterator()
  def nodes(ts: Int) = 0
  def filterTrie(ts: Int, f: (Int, Int) => Boolean, modified: List[Int], depth: Int) = MDD0
  def fillFound(ts: Int, f: (Int, Int) => Boolean, depth: Int, l: ListWithMax) {
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
  def forSubtries(f: (Int, MDD) => Boolean) {
    throw new UnsupportedOperationException
  }
  override def isEmpty = true
}

final class MDD1(private val child: MDD, private val index: Int) extends MDD {
  assert(child ne MDD0)
  var timestamp: Int = _

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
      } else if (v < index) {
        new MDD2(MDD0.addTrie(t, i + 1), v, child, index)
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

  override def hashCode: Int = 31 * index + child.hashCode

  override def equals(o: Any): Boolean = o match {
    case t: MDD1 => t.index == index && (t.child eq child)
    case _ => false
  }

  def fillFound(ts: Int, f: (Int, Int) => Boolean, depth: Int, l: ListWithMax) {
    if (timestamp != ts) {
      timestamp = ts
      if (depth <= l.max) {
        if (f(depth, index)) l.clear(depth)
        child.fillFound(ts, f, depth + 1, l)
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

  def nodes(ts: Int): Int = {
    if (ts == timestamp) {
      0
    } else {
      timestamp = ts
      1 + child.nodes(ts)
    }
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
  var timestamp: Int = _
  def addTrie(t: Array[Int], i: Int): MDD =
    if (i >= t.length) {
      MDDLeaf
    } else {
      t(i) match {
        case `leftI` => new MDD2(left.addTrie(t, i + 1), leftI, right, rightI)
        case `rightI` => new MDD2(left, leftI, right.addTrie(t, i + 1), rightI)
        case v: Int if v < leftI =>
          new MDD3(MDD0.addTrie(t, i + 1), v, left, leftI, right, rightI)

        case v: Int if v < rightI =>
          new MDD3(left, leftI, MDD0.addTrie(t, i + 1), v, right, rightI)

        case v: Int =>
          new MDD3(left, leftI, right, rightI, MDD0.addTrie(t, i + 1), v)

      }
    }

  def contains(t: Array[Int], i: Int): Boolean = t(i) match {
    case `leftI` => left.contains(t, i + 1)
    case `rightI` => right.contains(t, i + 1)
    case _ => false
  }

  def fillFound(ts: Int, f: (Int, Int) => Boolean, depth: Int, l: ListWithMax) {
    if (timestamp != ts) {
      timestamp = ts
      if (depth <= l.max) {
        if (f(depth, leftI)) l.clear(depth)
        left.fillFound(ts, f, depth + 1, l)
        if (depth <= l.max) {
          if (f(depth, rightI)) l.clear(depth)
          right.fillFound(ts, f, depth + 1, l)
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

  def listIterator: Iterator[List[Int]] = left.listIterator.map(0 :: _) ++ right.listIterator.map(1 :: _)
  def nodes(ts: Int): Int = if (ts == timestamp) {
    0
  } else {
    timestamp = ts
    1 + left.nodes(ts) + right.nodes(ts)
  }

  def reduce(mdds: collection.mutable.Map[Seq[MDD], MDD]): MDD = {
    var bL = left.reduce(mdds)
    var bR = right.reduce(mdds)

    val nT = MDD.newTrie((leftI, bL), (rightI, bR))

    mdds.getOrElseUpdate(nT, new MDD2(bL, leftI, bR, rightI))
  }

  override def hashCode: Int = List(left, leftI, right, rightI).hashCode

  override def equals(o: Any): Boolean = o match {
    case t: MDD2 => (left eq t.left) && (right eq t.right) && (leftI == t.leftI) && (rightI == t.rightI)
    case _ => false
  }

  override def isEmpty = false
}

final class MDD3(
  private val left: MDD, private val leftI: Int,
  private val mid: MDD, private val midI: Int,
  private val right: MDD, private val rightI: Int) extends MDD {
  assert(right ne MDD0)
  assert(mid ne MDD0)
  assert(left ne MDD0)
  assert(leftI < midI)
  assert(midI < rightI)

  def forSubtries(f: (Int, MDD) => Boolean) {
    f(leftI, left) && f(midI, mid) && f(rightI, right)
  }
  var timestamp: Int = _
  def addTrie(t: Array[Int], i: Int): MDD =
    if (i >= t.length) {
      MDDLeaf
    } else {
      t(i) match {
        case `leftI` => new MDD3(left.addTrie(t, i + 1), leftI, mid, midI, right, rightI)
        case `midI` => new MDD3(left, leftI, mid.addTrie(t, i + 1), midI, right, rightI)
        case `rightI` => new MDD3(left, leftI, mid, midI, right.addTrie(t, i + 1), rightI)
        case v: Int => {
          val newArray = MDD.newTrie((leftI, left), (midI, mid), (rightI, right), (v, MDD0.addTrie(t, i + 1)))
          new MDDn(newArray)
        }
      }
    }

  def contains(t: Array[Int], i: Int): Boolean = t(i) match {
    case `leftI` => left.contains(t, i + 1)
    case `midI` => mid.contains(t, i + 1)
    case `rightI` => right.contains(t, i + 1)
    case _ => false
  }

  def fillFound(ts: Int, f: (Int, Int) => Boolean, depth: Int, l: ListWithMax) {
    if (timestamp != ts) {
      timestamp = ts
      if (depth <= l.max) {
        if (f(depth, leftI)) l.clear(depth)
        left.fillFound(ts, f, depth + 1, l)

        if (depth <= l.max) {
          if (f(depth, midI)) l.clear(depth)
          mid.fillFound(ts, f, depth + 1, l)

          if (depth <= l.max) {
            if (f(depth, rightI)) l.clear(depth)
            right.fillFound(ts, f, depth + 1, l)
          }
        }
      }
    }
  }

  private var filteredResult: MDD = _

  private def filteredTrie(ts: Int, f: (Int, Int) => Boolean, modified: List[Int], depth: Int, t: MDD, i: Int) = {
    if (f(depth, i)) {
      t.filterTrie(ts, f, modified, depth + 1)
    } else {
      MDD0
    }
  }

  private def passedTrie(ts: Int, f: (Int, Int) => Boolean, modified: List[Int], depth: Int, t: MDD) = {
    t.filterTrie(ts, f, modified, depth + 1)
  }

  def filterTrie(ts: Int, f: (Int, Int) => Boolean, modified: List[Int], depth: Int): MDD =
    if (modified.isEmpty) {
      this
    } else if (ts == timestamp) {
      filteredResult
    } else {
      timestamp = ts

      var nL: MDD = null
      var nM: MDD = null
      var nR: MDD = null

      if (modified.head == depth) {
        // Some change at this level
        nL = filteredTrie(ts, f, modified.tail, depth, left, leftI)
        nM = filteredTrie(ts, f, modified.tail, depth, mid, midI)
        nR = filteredTrie(ts, f, modified.tail, depth, right, rightI)

      } else {
        // No change at this level (=> no need to call f())
        nL = passedTrie(ts, f, modified, depth, left)
        nM = passedTrie(ts, f, modified, depth, mid)
        nR = passedTrie(ts, f, modified, depth, right)
      }

      filteredResult =
        if (nL eq MDD0) {
          if (nM eq MDD0) {
            if (nR eq MDD0) {
              MDD0
            } else {
              new MDD1(nR, rightI)
            }
          } else if (nR eq MDD0) {
            new MDD1(nM, midI)
          } else {
            new MDD2(nM, midI, nR, rightI)
          }
        } else if (nM eq MDD0) {
          if (nR eq MDD0) {
            new MDD1(nL, leftI)
          } else {
            new MDD2(nL, leftI, nR, rightI)
          }
        } else {
          if (nR eq MDD0) {
            new MDD2(nL, leftI, nM, midI)
          } else {
            if ((nL eq left) && (nM eq mid) && (nR eq right)) {
              this
            } else {
              new MDD3(nL, leftI, nM, midI, nR, rightI)
            }
          }
        }

      filteredResult

    }

  def find(ts: Int, f: (Int, Int) => Boolean, depth: Int): Option[List[Int]] = {
    if (timestamp == ts) {
      None
    } else {
      timestamp = ts
      check(ts, f, depth, leftI, left).orElse(check(ts, f, depth, midI, mid)).orElse(check(ts, f, depth, rightI, right))
    }
  }

  private def check(ts: Int, f: (Int, Int) => Boolean, depth: Int, direction: Int, trie: MDD): Option[List[Int]] = {
    if (f(depth, direction)) {
      trie.find(ts, f, depth + 1).map(direction :: _)
    } else {
      None
    }
  }

  def listIterator: Iterator[List[Int]] = left.listIterator.map(leftI :: _) ++
    mid.listIterator.map(midI :: _) ++
    right.listIterator.map(rightI :: _)

  def nodes(ts: Int): Int = if (ts == timestamp) {
    0
  } else {
    timestamp = ts
    1 + left.nodes(ts) + mid.nodes(ts) + right.nodes(ts)
  }

  def reduce(mdds: collection.mutable.Map[Seq[MDD], MDD]): MDD = {
    val bL = left.reduce(mdds)
    val bM = mid.reduce(mdds)
    val bR = right.reduce(mdds)

    val nT = MDD.newTrie((leftI, bL), (midI, bM), (rightI, bR))
    mdds.getOrElseUpdate(nT, new MDD3(bL, leftI, bM, midI, bR, rightI))
  }

  override def hashCode: Int = List(left, leftI, mid, midI, right, rightI).hashCode

  override def equals(o: Any): Boolean = o match {
    case t: MDD3 =>
      (left eq t.left) && (right eq t.right) &&
        (mid eq t.mid) && (leftI == t.leftI) &&
        (midI == t.midI) && (rightI == t.rightI)
    case _ => false
  }

  override def isEmpty = false
}

final class MDDn(private val trie: Array[MDD]) extends MDD {
  var timestamp = 0

  def forSubtries(f: (Int, MDD) => Boolean) {
    forSubtries(f, trie.length - 1)
  }

  @tailrec
  def forSubtries(f: (Int, MDD) => Boolean, i: Int) {
    if (i >= 0) {
      val t = trie(i)
      if ((t eq MDD0) || f(i, t)) {
        forSubtries(f, i - 1)
      }
    }
  }

  def addTrie(tuple: Array[Int], i: Int): MDD = {
    if (i >= tuple.length) {
      MDDLeaf
    } else {
      val v = tuple(i)
      val newTrie = trie.padTo(v + 1, MDD0)
      newTrie(v) = newTrie(v).addTrie(tuple, i + 1)
      new MDDn(newTrie)
    }
  }

  def reduce(mdds: collection.mutable.Map[Seq[MDD], MDD]): MDD = {
    var b = trie.map(_.reduce(mdds))
    mdds.getOrElseUpdate(b, new MDDn(b))
  }

  //@tailrec
  def contains(tuple: Array[Int], i: Int): Boolean =
    tuple(i) < trie.length && trie(tuple(i)).contains(tuple, i + 1)

  def find(ts: Int, f: (Int, Int) => Boolean, depth: Int): Option[List[Int]] = {
    if (timestamp == ts) {
      None
    } else {
      timestamp = ts
      findHere(ts, f, depth)
    }
  }

  @tailrec
  private def findHere(ts: Int, f: (Int, Int) => Boolean, depth: Int, i: Int = trie.length - 1): Option[List[Int]] = {
    if (i < 0) {
      None
    } else if (f(depth, i)) {
      trie(i).find(ts, f, depth + 1) match {
        case Some(found) => Some(i :: found)
        case None => findHere(ts, f, depth, i - 1)
      }
    } else {
      findHere(ts, f, depth, i - 1)
    }
  }

  override lazy val hashCode: Int = MurmurHash3.arrayHash(trie)

  override def equals(o: Any): Boolean = o match {
    case t: MDDn =>
      val len = t.trie.length
      len == trie.length && {
        var i = len - 1
        while (i >= 0 && (t.trie(i) eq trie(i))) i -= 1
        i < 0
      }
    case _ => false
  }

  def fillFound(ts: Int, f: (Int, Int) => Boolean, depth: Int, l: ListWithMax) {
    if (timestamp != ts) {
      timestamp = ts
      var i = trie.length - 1
      while (i >= 0 && depth <= l.max) {
        if (trie(i) ne MDD0) {
          if (f(depth, i)) l.clear(depth)
          trie(i).fillFound(ts, f, depth + 1, l)
        }
        i -= 1
      }
    }
  }

  private var filteredResult: MDD = _

  private def udTrie0(trie: Array[MDD], i: Int) {
    if (trie ne null) {
      trie(i) = MDD0
    }
  }

  private def filteredTrie(ts: Int, f: (Int, Int) => Boolean, modified: List[Int], depth: Int) = {

    val trie = this.trie
    var newTrie: Array[MDD] = null
    var i = trie.length - 1
    while (i >= 0) {
      if ((trie(i) ne MDD0) && f(depth, i)) {
        val uT = trie(i).filterTrie(ts, f, modified, depth + 1)
        if (uT ne MDD0) {
          if (newTrie eq null) {
            newTrie = fill(i + 1, uT)
          } else {
            newTrie(i) = uT
          }

        }
      }
      i -= 1
    }
    newTrie
  }

  private def passedTrie(ts: Int, f: (Int, Int) => Boolean, modified: List[Int], depth: Int): Array[MDD] = {
    val trie = this.trie
    var newTrie: Array[MDD] = null
    var i = trie.length - 1
    while (i >= 0) {
      val nT = trie(i).filterTrie(ts, f, modified, depth)
      if (nT ne MDD0) {
        if (newTrie eq null) {
          newTrie = fill(i + 1, nT)
        } else {
          newTrie(i) = nT
        }
      }
      i -= 1
    }
    newTrie
  }

  private def fill(s: Int, last: MDD) = {
    val t = new Array[MDD](s)
    var i = s - 1
    t(i) = last
    i -= 1
    while (i >= 0) {
      t(i) = MDD0
      i -= 1
    }
    t
  }

  def filterTrie(ts: Int, f: (Int, Int) => Boolean, modified: List[Int], depth: Int = 0): MDD =
    if (modified.isEmpty) {
      this
    } else if (ts == timestamp) {
      filteredResult
    } else {
      timestamp = ts

      val newTrie =
        if (modified.head == depth) {
          // Some change at this level
          filteredTrie(ts, f, modified.tail, depth)
        } else {
          // No change at this level (=> no need to call f())
          passedTrie(ts, f, modified, depth + 1)
        }

      filteredResult =
        if (newTrie eq null) {
          MDD0
        } else if (same(newTrie, trie)) {
          this
        } else {
          MDD.newNode(newTrie)
        }
      filteredResult

    }

  private def same(t1: Array[MDD], t2: Array[MDD]): Boolean = {
    if (t1.length != t2.length) {
      false
    } else {
      var i = t1.length - 1
      while (i >= 0) {
        if (t1(i) ne t2(i)) {
          return false
        }
        i -= 1
      }
      true
    }
  }

  def listIterator = trie.iterator.zipWithIndex flatMap {
    case (t, i) => t.listIterator map (i :: _)
  }

  def nodes(ts: Int): Int = {
    if (ts == timestamp) {
      0
    } else {
      timestamp = ts
      1 + trie.map(_.nodes(ts)).sum
    }
  }

  override def isEmpty = false

}


