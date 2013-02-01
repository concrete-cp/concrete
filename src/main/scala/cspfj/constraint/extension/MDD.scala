package cspfj.constraint.extension

import java.util.Arrays
import scala.annotation.tailrec
import scala.collection.mutable.HashMap
import scala.collection.mutable.Seq
import cspfj.Statistic
import scala.collection.mutable.WeakHashMap
import scala.util.hashing.MurmurHash3
import cspfj.util.ListWithMax

trait RelationGenerator {
  def apply(data: Iterator[Array[Int]]): Relation
}

object MDD extends RelationGenerator {
  def apply(data: Array[Int]*): MDD = apply(data.iterator)

  def apply(data: Iterator[Array[Int]]): MDD = {
    val root = data.foldLeft[MDDNode](MDD0) {
      (acc, tuple) => acc + tuple
    }
    val reduced = root.reduce(new HashMap[Seq[MDDNode], MDDNode]())
    new MDD(reduced)
  }

  var timestamp = 0

  def newNode(t: Array[MDDNode], size: Int): MDDNode = {
    var found1: (Int, MDDNode) = null
    var found2: (Int, MDDNode) = null
    var found3: (Int, MDDNode) = null
    var i = t.length - 1
    while (i >= 0) {
      if (t(i) ne MDD0) {
        if (found1 eq null) {
          found1 = (i, t(i))
        } else if (found2 eq null) {
          found2 = (i, t(i))
        } else if (found3 eq null) {
          found3 = (i, t(i))
        } else {
          return new MDDn(t, size)
        }
      }
      i -= 1
    }

    val (i1, t1) = found1
    found2 match {
      case null => new MDD1(t1, i1)
      case (i2, t2) => found3 match {
        case null => new MDD2(t2, i2, t1, i1, size)
        case (i3, t3) => new MDD3(t3, i3, t2, i2, t1, i1, size)
      }

    }
  }

}

final class MDD(val root: MDDNode) extends Relation {

  type Self2 = MDD

  def this() = this(MDD0)

  override def size = root.size

  def +(t: Array[Int]) = throw new UnsupportedOperationException

  def -(t: Array[Int]) = {
    throw new UnsupportedOperationException
  }

  def filterTrie(f: (Int, Int) => Boolean, modified: List[Int]) = {
    MDD.timestamp += 1
    new MDD(root.filterTrie(MDD.timestamp, f, modified, 0))
  }

  def contains(t: Array[Int]) = root.contains(t)

  def iterator = root.listIterator.map(_.toArray)

  def tupleString = iterator map { _.mkString(" ") } mkString "|"

  def fillFound(f: (Int, Int) => Boolean, arity: Int) = {
    MDD.timestamp += 1
    val l = new ListWithMax(arity)
    root.fillFound(MDD.timestamp, f, 0, l)
    l
  }

  def nodes = {
    MDD.timestamp += 1
    root.nodes(MDD.timestamp)
  }

  def find(f: (Int, Int) => Boolean) = {
    MDD.timestamp += 1
    root.find(MDD.timestamp, f, 0) map (_.toArray)
  }

  override def toString = nodes + " nodes representing " + size + " tuples"

  def copy = this
}

trait MDDNode {
  def size: Int
  def +(t: Array[Int]): MDDNode = if (contains(t)) this else addTrie(t, 0)
  def addTrie(t: Array[Int], i: Int): MDDNode
  def reduce(mdds: collection.mutable.Map[Seq[MDDNode], MDDNode]): MDDNode
  def contains(t: Array[Int]): Boolean = contains(t, 0)
  def contains(t: Array[Int], i: Int): Boolean
  def find(ts: Int, f: (Int, Int) => Boolean, depth: Int): Option[List[Int]]
  def listIterator: Iterator[List[Int]]
  def nodes(ts: Int): Int
  def filterTrie(ts: Int, f: (Int, Int) => Boolean, modified: List[Int], depth: Int): MDDNode
  def fillFound(ts: Int, f: (Int, Int) => Boolean, depth: Int, l: ListWithMax)
}

final object MDDLeaf extends MDDNode {
  private var timestamp = 0
  def size = 1
  def reduce(mdds: collection.mutable.Map[Seq[MDDNode], MDDNode]) = this
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
}

final object MDD0 extends MDDNode {
  def size = 0
  def reduce(mdds: collection.mutable.Map[Seq[MDDNode], MDDNode]) = MDD0
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
}

final class MDD1(private val child: MDDNode, private val index: Int) extends MDDNode {
  require(child ne MDD0)
  private var timestamp: Int = _
  def addTrie(t: Array[Int], i: Int): MDDNode =
    if (i >= t.length) {
      MDDLeaf
    } else {
      val v = t(i)
      if (v == index) {
        new MDD1(child.addTrie(t, i + 1), index)
      } else {
        new MDD2(child, index, MDD0.addTrie(t, i + 1), v, size + 1)
      }
    }

  val size = child.size

  def contains(t: Array[Int], i: Int): Boolean = {
    t(i) == index && child.contains(t, i + 1)
  }

  def reduce(mdds: collection.mutable.Map[Seq[MDDNode], MDDNode]): MDDNode = {

    //    } else if (mdds.contains(trie)) {
    //      this
    //    } else

    var b = child.reduce(mdds)
    //
    //    if (b eq EmptyMDD) {
    //      EmptyMDD
    //    } else {
    val newArray = Array.fill[MDDNode](index + 1)(MDD0)
    newArray(index) = b
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

  override def toString = "MDD representing " + size + " tuples"

  def fillFound(ts: Int, f: (Int, Int) => Boolean, depth: Int, l: ListWithMax) {
    if (timestamp != ts) {
      timestamp = ts
      if (depth <= l.max) {
        if (f(depth, index)) l.clear(depth)
        child.fillFound(ts, f, depth + 1, l)
      }
    }
  }

  private var filteredResult: MDDNode = _

  def filterTrie(ts: Int, f: (Int, Int) => Boolean, modified: List[Int], depth: Int = 0): MDDNode =
    if (modified.isEmpty) {
      this
    } else if (ts == timestamp) {
      filteredResult
    } else {
      timestamp = ts

      val nC = if (modified.head == depth) {
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
  private val left: MDDNode, private val leftI: Int,
  private val right: MDDNode, private val rightI: Int,
  val size: Int) extends MDDNode {
  require(right ne MDD0)
  require(left ne MDD0)
  require(leftI < rightI)

  private var timestamp: Int = _
  def addTrie(t: Array[Int], i: Int): MDDNode =
    if (i >= t.length) {
      MDDLeaf
    } else {
      t(i) match {
        case `leftI` => new MDD2(left.addTrie(t, i + 1), leftI, right, rightI, size + 1)
        case `rightI` => new MDD2(left, leftI, right.addTrie(t, i + 1), rightI, size + 1)
        case v: Int => {
          val newArray = Array.fill[MDDNode](math.max(v, rightI) + 1)(MDD0)
          newArray(leftI) = left
          newArray(rightI) = right
          newArray(v) = newArray(v).addTrie(t, i + 1)
          new MDDn(newArray, size + 1)
        }
      }
    }

  def contains(t: Array[Int], i: Int): Boolean = t(i) match {
    case `leftI` => left.contains(t, i + 1)
    case `rightI` => right.contains(t, i + 1)
    case _ => false
  }

  def fillFound(ts: Int, f: (Int, Int) => Boolean, depth: Int, l: ListWithMax): Unit =
    if (timestamp != ts) {
      timestamp = ts
      if (depth <= l.max) {
        if (f(depth, leftI)) l.clear(depth)
        left.fillFound(ts, f, depth + 1, l)
      }
      if (depth <= l.max) {
        if (f(depth, rightI)) l.clear(depth)
        right.fillFound(ts, f, depth + 1, l)
      }
    }

  private var filteredResult: MDDNode = _

  private def filteredTrie(ts: Int, f: (Int, Int) => Boolean, modified: List[Int], depth: Int, t: MDDNode, i: Int) = {
    if (f(depth, i)) {
      t.filterTrie(ts, f, modified, depth + 1)
    } else {
      MDD0
    }
  }

  private def passedTrie(ts: Int, f: (Int, Int) => Boolean, modified: List[Int], depth: Int, t: MDDNode) = {
    t.filterTrie(ts, f, modified, depth + 1)
  }

  def filterTrie(ts: Int, f: (Int, Int) => Boolean, modified: List[Int], depth: Int): MDDNode =
    if (modified.isEmpty) {
      this
    } else if (ts == timestamp) {
      filteredResult
    } else {
      timestamp = ts

      var nL: MDDNode = null
      var nR: MDDNode = null

      if (modified.head == depth) {
        // Some change at this level
        nL = filteredTrie(ts, f, modified.tail, depth, left, leftI)
        nR = filteredTrie(ts, f, modified.tail, depth, right, rightI)

      } else {
        // No change at this level (=> no need to call f())
        nL = passedTrie(ts, f, modified, depth, left)
        nR = passedTrie(ts, f, modified, depth, right)
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
          val s = nL.size + nR.size
          if (s == size) {
            this
          } else {
            new MDD2(nL, leftI, nR, rightI, s)
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

  private def check(ts: Int, f: (Int, Int) => Boolean, depth: Int, direction: Int, trie: MDDNode): Option[List[Int]] = {
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

  def reduce(mdds: collection.mutable.Map[Seq[MDDNode], MDDNode]): MDDNode = {
    var bL = left.reduce(mdds)
    var bR = right.reduce(mdds)

    val nT = Array.fill[MDDNode](rightI + 1)(MDD0)
    nT(leftI) = bL
    nT(rightI) = bR
    mdds.getOrElseUpdate(nT, new MDD2(bL, leftI, bR, rightI, size))
  }

  override def hashCode: Int = {
    MurmurHash3.listHash(List(left, leftI, right, rightI), 0)
  }

  override def equals(o: Any): Boolean = o match {
    case t: MDD2 => (left eq t.left) && (right eq t.right) && (leftI == t.leftI) && (rightI == t.rightI)
    case _ => false
  }
}

final class MDD3(
  private val left: MDDNode, private val leftI: Int,
  private val mid: MDDNode, private val midI: Int,
  private val right: MDDNode, private val rightI: Int,
  val size: Int) extends MDDNode {
  require(right ne MDD0)
  require(mid ne MDD0)
  require(left ne MDD0)
  require(leftI < midI)
  require(midI < rightI)

  private var timestamp: Int = _
  def addTrie(t: Array[Int], i: Int): MDDNode =
    if (i >= t.length) {
      MDDLeaf
    } else {
      t(i) match {
        case `leftI` => new MDD3(left.addTrie(t, i + 1), leftI, mid, midI, right, rightI, size + 1)
        case `midI` => new MDD3(left, leftI, mid.addTrie(t, i + 1), midI, right, rightI, size + 1)
        case `rightI` => new MDD3(left, leftI, mid, midI, right.addTrie(t, i + 1), rightI, size + 1)
        case v: Int => {
          val newArray = Array.fill[MDDNode](math.max(v, rightI) + 1)(MDD0)
          newArray(leftI) = left
          newArray(midI) = mid
          newArray(rightI) = right
          newArray(v) = newArray(v).addTrie(t, i + 1)
          new MDDn(newArray, size + 1)
        }
      }
    }

  def contains(t: Array[Int], i: Int): Boolean = t(i) match {
    case `leftI` => left.contains(t, i + 1)
    case `midI` => mid.contains(t, i + 1)
    case `rightI` => right.contains(t, i + 1)
    case _ => false
  }

  def fillFound(ts: Int, f: (Int, Int) => Boolean, depth: Int, l: ListWithMax): Unit =
    if (timestamp != ts) {
      timestamp = ts
      if (depth <= l.max) {
        if (f(depth, leftI)) l.clear(depth)
        left.fillFound(ts, f, depth + 1, l)
      }
      if (depth <= l.max) {
        if (f(depth, midI)) l.clear(depth)
        mid.fillFound(ts, f, depth + 1, l)
      }
      if (depth <= l.max) {
        if (f(depth, rightI)) l.clear(depth)
        right.fillFound(ts, f, depth + 1, l)
      }
    }

  private var filteredResult: MDDNode = _

  private def filteredTrie(ts: Int, f: (Int, Int) => Boolean, modified: List[Int], depth: Int, t: MDDNode, i: Int) = {
    if (f(depth, i)) {
      t.filterTrie(ts, f, modified, depth + 1)
    } else {
      MDD0
    }
  }

  private def passedTrie(ts: Int, f: (Int, Int) => Boolean, modified: List[Int], depth: Int, t: MDDNode) = {
    t.filterTrie(ts, f, modified, depth + 1)
  }

  def filterTrie(ts: Int, f: (Int, Int) => Boolean, modified: List[Int], depth: Int): MDDNode =
    if (modified.isEmpty) {
      this
    } else if (ts == timestamp) {
      filteredResult
    } else {
      timestamp = ts

      var nL: MDDNode = null
      var nM: MDDNode = null
      var nR: MDDNode = null

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
            new MDD2(nM, midI, nR, rightI, nM.size + nR.size)
          }
        } else if (nM eq MDD0) {
          if (nR eq MDD0) {
            new MDD1(nL, leftI)
          } else {
            new MDD2(nL, leftI, nR, rightI, nL.size + nR.size)
          }
        } else {
          if (nR eq MDD0) {
            new MDD2(nL, leftI, nM, midI, nL.size + nM.size)
          } else {
            val s = nL.size + nM.size + nR.size
            if (s == size) {
              this
            } else {
              new MDD3(nL, leftI, nM, midI, nR, rightI, s)
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

  private def check(ts: Int, f: (Int, Int) => Boolean, depth: Int, direction: Int, trie: MDDNode): Option[List[Int]] = {
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

  def reduce(mdds: collection.mutable.Map[Seq[MDDNode], MDDNode]): MDDNode = {
    val bL = left.reduce(mdds)
    val bM = mid.reduce(mdds)
    val bR = right.reduce(mdds)

    val nT = Array.fill[MDDNode](rightI + 1)(MDD0)
    nT(leftI) = bL
    nT(midI) = bM
    nT(rightI) = bR
    mdds.getOrElseUpdate(nT, new MDD3(bL, leftI, bM, midI, bR, rightI, size))
  }

  override def hashCode: Int = {
    MurmurHash3.listHash(List(left, leftI, mid, midI, right, rightI), 0)
  }

  override def equals(o: Any): Boolean = o match {
    case t: MDD3 =>
      (left eq t.left) && (right eq t.right) &&
        (mid eq t.mid) && (leftI == t.leftI) &&
        (midI == t.midI) && (rightI == t.rightI)
    case _ => false
  }
}

final class MDDn(private val trie: Array[MDDNode], val size: Int) extends MDDNode {
  require(size > 0)
  private var timestamp = 0

  def addTrie(tuple: Array[Int], i: Int): MDDNode = {
    if (i >= tuple.length) {
      MDDLeaf
    } else {
      val v = tuple(i)
      val newArray = trie.padTo(v + 1, MDD0)
      newArray(v) = newArray(v).addTrie(tuple, i + 1)
      new MDDn(newArray, size + 1)
    }
  }

  def reduce(mdds: collection.mutable.Map[Seq[MDDNode], MDDNode]): MDDNode = {
    var b = trie.map(_.reduce(mdds))
    mdds.getOrElseUpdate(b, new MDDn(b, size))
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

  override def toString = "MDD representing " + size + " tuples"
  //
  //  def foreachTrie(f: (Int, Int) => Unit, depth: Int = 0) {
  //    var i = trie.length - 1;
  //    while (i >= 0) {
  //      if (trie(i) ne null) {
  //        f(depth, i)
  //        trie(i).foreachTrie(f, depth + 1)
  //      }
  //      i -= 1
  //    }
  //  }

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

  private var filteredResult: MDDNode = _

  private def filteredTrie(ts: Int, f: (Int, Int) => Boolean, modified: List[Int], depth: Int, newTrie: Array[MDDNode]) = {
    var newSize = 0
    val trie = this.trie
    var i = trie.length - 1
    while (i >= 0) {
      if ((trie(i) ne MDD0) && f(depth, i)) {
        newTrie(i) = trie(i).filterTrie(ts, f, modified, depth + 1)
        newSize += newTrie(i).size
      } else {
        newTrie(i) = MDD0
      }
      i -= 1
    }
    newSize
  }

  private def passedTrie(ts: Int, f: (Int, Int) => Boolean, modified: List[Int], depth: Int, newTrie: Array[MDDNode]) = {
    var newSize = 0
    val trie = this.trie
    var i = trie.length - 1
    while (i >= 0) {
      newTrie(i) = trie(i).filterTrie(ts, f, modified, depth)
      newSize += newTrie(i).size
      i -= 1
    }
    newSize
  }

  def filterTrie(ts: Int, f: (Int, Int) => Boolean, modified: List[Int], depth: Int = 0): MDDNode =
    if (modified.isEmpty) {
      this
    } else if (ts == timestamp) {
      filteredResult
    } else {
      timestamp = ts

      val newTrie = new Array[MDDNode](trie.length)
      var newSize = 0

      if (modified.head == depth) {
        // Some change at this level
        newSize = filteredTrie(ts, f, modified.tail, depth, newTrie)
      } else {
        // No change at this level (=> no need to call f())
        newSize = passedTrie(ts, f, modified, depth + 1, newTrie)
      }

      filteredResult = newSize match {
        case 0 => MDD0
        case `size` => this
        case s: Int => MDD.newNode(newTrie, s)
      }
      filteredResult

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

}

