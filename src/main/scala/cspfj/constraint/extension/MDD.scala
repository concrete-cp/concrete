package cspfj.constraint.extension

import scala.annotation.tailrec
import scala.collection.mutable.HashMap
import scala.collection.mutable.Seq
import scala.util.hashing.MurmurHash3
import cspfj.priorityqueues.Identified
import cspfj.util.ListWithMax
import java.util.Arrays

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

class ABuffer(var array: Array[Int]) {

  def this(s: Int) = this(new Array[Int](s))

  private var s = 0
  def size = s
  def clearNewIndices() {
    s = 0
  }
  def add(i: Int) {
    array(s) = i
    s += 1
  }
  def toArray = {
    //val array = new Array[Int](nbNewIndices)
    Arrays.copyOf(array, s) //System.arrayCopy(newIndices, 0, array, 0, nbNewIndices)
    //array

  }
  def apply(i: Int) = array(i)
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
  def nodes(ts: Int) = 0
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

  def nodes(ts: Int): Int = {
    if (ts == timestamp) {
      0
    } else {
      timestamp = ts
      1 + child.nodes(ts)
    }
  }

  override def isEmpty = false
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

  def fillFound(ts: Int, f: (Int, Int) => Boolean, depth: Int, l: ListWithMax) {
    if (timestamp != ts) {
      timestamp = ts
      if (depth <= l.max) {
        if (f(depth, leftI)) l.clear(depth)
        if (depth + 1 <= l.max) {
          left.fillFound(ts, f, depth + 1, l)
        }
        if (depth <= l.max) {
          if (f(depth, rightI)) l.clear(depth)
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

  def listIterator: Iterator[List[Int]] = left.listIterator.map(0 :: _) ++ right.listIterator.map(1 :: _)
  def nodes(ts: Int): Int = if (ts == timestamp) {
    0
  } else {
    timestamp = ts
    2 + left.nodes(ts) + right.nodes(ts)
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

final class MDDn(private val trie: Array[MDD], private val indices: Array[Int], private val nbIndices: Int) extends MDD {
  var timestamp = 0

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
      val newTrie = trie.padTo(v + 1, MDD0)
      val newInd =
        if (newTrie(v) eq MDD0) {
          indices :+ v
        } else {
          indices
        }
      newTrie(v) = newTrie(v).addTrie(tuple, i + 1)

      new MDDn(newTrie, newInd, newInd.length)
    }
  }

  def reduce(mdds: collection.mutable.Map[Seq[MDD], MDD]): MDD = {
    var b = trie.map(_.reduce(mdds))
    mdds.getOrElseUpdate(b, new MDDn(b, indices, nbIndices))
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

  override lazy val hashCode: Int = {
    //val hash = new MurmurHash3
    MurmurHash3.unorderedHash(indices.iterator.take(nbIndices).map(i => (i, trie(i))))
  }

  override def equals(o: Any): Boolean = o match {
    case t: MDDn =>
      val len = t.nbIndices
      len == nbIndices && {
        var i = len - 1
        while (i >= 0 && t.trie.length > indices(i) && (t.trie(indices(i)) eq trie(indices(i))) &&
          trie.length > t.indices(i) && (t.trie(t.indices(i)) eq trie(t.indices(i)))) {
          i -= 1
        }
        i < 0
      }
    case _ => false
  }

  def fillFound(ts: Int, f: (Int, Int) => Boolean, depth: Int, l: ListWithMax) {
    if (timestamp != ts) {
      timestamp = ts
      var i = nbIndices - 1
      while (i >= 0 && depth <= l.max) {
        val ti = indices(i)
        if (f(depth, ti)) l.clear(depth)
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

  def newNode(t: Array[MDD], nbIndices: Int): MDD = {
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

  def nodes(ts: Int): Int = {
    if (ts == timestamp) {
      0
    } else {
      timestamp = ts
      nbIndices + indices.take(nbIndices).map(trie(_)).map(_.nodes(ts)).sum
    }
  }

  override def isEmpty = false

}


