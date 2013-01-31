package cspfj.constraint.extension

import java.util.Arrays
import scala.annotation.tailrec
import scala.collection.mutable.HashMap
import scala.collection.mutable.Seq
import cspfj.Statistic
import scala.collection.mutable.WeakHashMap
import scala.util.hashing.MurmurHash3

trait RelationGenerator {
  def apply(data: Iterator[Array[Int]]): Relation
}

object MDD extends RelationGenerator {
  def apply(data: Array[Int]*): MDD = apply(data.iterator)

  //  def apply(data: Iterator[Array[Int]]): MDD = {
  //    val mdds = new HashMap[Seq[MDDNode], MDDNode]()
  //    var it = 1000
  //    val m = new MDD(data.foldLeft(empty) { (acc, tuple) =>
  //      if (mdds.size >= it) {
  //        mdds.clear()
  //        timestamp += 1
  //        acc.renew(timestamp, mdds)
  //        it = mdds.size + 1000
  //      }
  //      acc + (mdds, tuple)
  //    })
  //    mdds.clear()
  //    m
  //    //println(mdds.map(_._2.hashCode).toSeq.sorted)
  //
  //  }

  def apply(data: Iterator[Array[Int]]): MDD = {
    val root = data.foldLeft[MDDNode](EmptyMDD) {
      (acc, tuple) => acc.addTrie(tuple)
    }
    val reduced = root.reduce(new HashMap[Seq[MDDNode], MDDNode]())
    new MDD(reduced)
  }

  //val leaf = new MDDNode(Array(), 1)

  //val empty = new MDDNode(Array(), 0)

  var timestamp = 0

}

final class MDD(val root: MDDNode) extends Relation {

  type Self2 = MDD

  def this() = this(EmptyMDD)

  override def size = root.size

  def +(t: Array[Int]) = {
    throw new UnsupportedOperationException
  }

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

final object EmptyMDD extends MDDNode {
  def size = 0
  def reduce(mdds: collection.mutable.Map[Seq[MDDNode], MDDNode]) = this
  def contains(tuple: Array[Int], i: Int) = false
  def find(ts: Int, f: (Int, Int) => Boolean, depth: Int) = None
  def listIterator = Iterator()
  def nodes(ts: Int) = 0
  def filterTrie(ts: Int, f: (Int, Int) => Boolean, modified: List[Int], depth: Int) = this
  def fillFound(ts: Int, f: (Int, Int) => Boolean, depth: Int, l: ListWithMax) {
    throw new UnsupportedOperationException
  }
  def trie = throw new UnsupportedOperationException
  def addTrie(tuple: Array[Int], i: Int) = {
    if (i >= tuple.length) {
      MDDLeaf
    } else {
      val v = tuple(i)
      val newArray = Array.fill[MDDNode](v + 1)(EmptyMDD)
      newArray(v) = newArray(v).addTrie(tuple, i + 1)
      new FullMDDNode(newArray, 1)
    }
  }
  def eqTrie(n: MDDNode) = n eq EmptyMDD
  def filterNode(f: (Int, Int) => Boolean, depth: Int, value: Int) = EmptyMDD
}

final object MDDLeaf extends MDDNode {
  var timestamp = 0
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
  def trie = throw new UnsupportedOperationException
  def addTrie(tuple: Array[Int], i: Int) = throw new UnsupportedOperationException
  def eqTrie(n: MDDNode) = n eq MDDLeaf
  def filterNode(f: (Int, Int) => Boolean, depth: Int, i: Int) =
    if (f(depth, i)) this else EmptyMDD

}

trait MDDNode {
  def trie: Array[MDDNode]
  def size: Int
  def addTrie(t: Array[Int]): MDDNode = if (contains(t)) this else addTrie(t, 0)
  def addTrie(t: Array[Int], i: Int): MDDNode
  def reduce(mdds: collection.mutable.Map[Seq[MDDNode], MDDNode]): MDDNode
  def contains(t: Array[Int]): Boolean = contains(t, 0)
  def contains(t: Array[Int], i: Int): Boolean
  def find(ts: Int, f: (Int, Int) => Boolean, depth: Int): Option[List[Int]]
  def listIterator: Iterator[List[Int]]
  def nodes(ts: Int): Int
  def filterTrie(ts: Int, f: (Int, Int) => Boolean, modified: List[Int], depth: Int): MDDNode
  def fillFound(ts: Int, f: (Int, Int) => Boolean, depth: Int, l: ListWithMax)
  def eqTrie(n: MDDNode): Boolean
  def filterNode(f: (Int, Int) => Boolean, depth: Int, value: Int): MDDNode
}

final class FullMDDNode(val trie: Array[MDDNode], val size: Int) extends MDDNode {
  require(size > 0)
  var timestamp = 0

  def isEmpty = false

  //  def +(mdds: collection.mutable.Map[Seq[MDDNode], MDDNode], t: Array[Int]): MDDNode =
  //    if (contains(t)) this else this + (mdds, t, 0)
  //
  //  private def +(mdds: collection.mutable.Map[Seq[MDDNode], MDDNode], tuple: Array[Int], i: Int): MDDNode =
  //    if (i >= tuple.length) { MDD.leaf }
  //    else {
  //      val v = tuple(i)
  //      val newArray = trie.padTo(v + 1, EmptyMDD)
  //      //ensureCapacity(v + 1)
  //
  //      newArray(v) += (mdds, tuple, i + 1)
  //
  //      mdds.getOrElseUpdate(newArray, new MDDNode(newArray, size + 1))
  //    }

  def addTrie(tuple: Array[Int], i: Int): MDDNode = {
    if (i >= tuple.length) {
      MDDLeaf
    } else {
      val v = tuple(i)
      val newArray = trie.padTo(v + 1, EmptyMDD)
      newArray(v) = newArray(v).addTrie(tuple, i + 1)
      new FullMDDNode(newArray, size + 1)
    }
  }

  def reduce(mdds: collection.mutable.Map[Seq[MDDNode], MDDNode]): MDDNode = {

    //    } else if (mdds.contains(trie)) {
    //      this
    //    } else

    var b = trie.map(_.reduce(mdds))

    if (b.forall(_ eq EmptyMDD)) {
      EmptyMDD
    } else {
      mdds.getOrElseUpdate(b, new FullMDDNode(b, size))
    }

  }

  //@tailrec
  def contains(tuple: Array[Int], i: Int): Boolean =
    tuple(i) < trie.length && trie(tuple(i)).contains(tuple, i + 1)

  //  def renew(ts: Int, mdds: collection.mutable.Map[Seq[MDDNode], MDDNode]) {
  //    if (timestamp != ts) {
  //      timestamp = ts
  //      mdds.put(trie, this)
  //      trie.foreach(t => if (t ne null) t.renew(ts, mdds))
  //    }
  //  }

  //var latest: Option[List[Int]] = None

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
      val found = trie(i).find(ts, f, depth + 1)
      if (found.isDefined) {
        Some(i :: found.get)
      } else {
        findHere(ts, f, depth, i - 1)
      }
    } else {
      findHere(ts, f, depth, i - 1)
    }
  }

  override lazy val hashCode: Int = MurmurHash3.arrayHash(trie)

  override def equals(o: Any): Boolean = o match {
    case t: FullMDDNode => eqTrie(t)
    case _ => false
  }

  def eqTrie(n: MDDNode): Boolean = {
    val len = n.trie.length
    len == trie.length && {
      var i = 0
      while (i < len && (n.trie(i) eq trie(i))) i += 1
      i == len
    }
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
        if (trie(i) ne EmptyMDD) {
          if (f(depth, i)) l.clear(depth)
          trie(i).fillFound(ts, f, depth + 1, l)
        }
        i -= 1
      }
    }
  }

  var filteredResult: MDDNode = _

  def filterNode(f: (Int, Int) => Boolean, depth: Int, v: Int): MDDNode =
    if (f(depth, v)) this else EmptyMDD

  private def filteredTrie(ts: Int, f: (Int, Int) => Boolean, modified: List[Int], depth: Int, newTrie: Array[MDDNode]) = {
    var newSize = 0
    var i = trie.length - 1
    while (i >= 0) {
      if ((trie(i) ne EmptyMDD) && f(depth, i)) {
        newTrie(i) = trie(i).filterTrie(ts, f, modified, depth + 1)
        newSize += newTrie(i).size
      } else {
        newTrie(i) = EmptyMDD
      }
      i -= 1
    }
    newSize
  }

  private def passedTrie(ts: Int, f: (Int, Int) => Boolean, modified: List[Int], depth: Int, newTrie: Array[MDDNode]) = {
    var newSize = 0
    var i = trie.length - 1
    while (i >= 0) {
      newTrie(i) = trie(i).filterTrie(ts, f, modified, depth + 1)
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
      val newSize = if (modified.head == depth) {
        // Some change at this level
        filteredTrie(ts, f, modified.tail, depth, newTrie)
      } else {
        // No change at this level (=> no need to call f())
        passedTrie(ts, f, modified, depth, newTrie)
      }

      filteredResult = newSize match {
        case 0 => EmptyMDD
        case `size` => this
        case s: Int => new FullMDDNode(newTrie, s)
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

