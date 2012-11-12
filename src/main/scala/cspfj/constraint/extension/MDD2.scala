package cspfj.constraint.extension

import java.util.Arrays
import scala.annotation.tailrec
import scala.collection.mutable.HashMap
import scala.collection.mutable.Seq
import cspfj.Statistic

object MDD2 {
  def apply(data: Array[Int]*) = {
    val mdds = new HashMap[Seq[MDD2Node], MDD2Node]()
    new MDD2(data.foldLeft(empty)(_ + (mdds, _)))
  }

  val leaf = new MDD2Node(Array(), 1)

  val empty = new MDD2Node(Array(), 0)

  var timestamp = 0

}

final class MDD2(val root: MDD2Node) extends Relation {

  type Self2 = MDD2

  override def size = root.size

  def +(t: Array[Int]) = {
    throw new UnsupportedOperationException
  }

  def -(t: Array[Int]) = {
    throw new UnsupportedOperationException
  }

  def filterTrie(f: (Int, Int) => Boolean, modified: List[Int]) = {
    MDD2.timestamp += 1
    root.filterTrie(MDD2.timestamp, f, modified, 0)
    assert(root.size == root.computeSize, root.size + " != " + root.computeSize + "\n" + root.toString)
    this
  }

  def contains(t: Array[Int]) = root.contains(t)

  def iterator = root.listiterator(this).map(_.toArray)

  def tupleString = iterator map { _.mkString(" ") } mkString "|"

  def fillFound(f: (Int, Int) => Boolean, arity: Int) = {
    MDD2.timestamp += 1
    val l = new ListWithMax(arity)
    root.fillFound(MDD2.timestamp, f, 0, l)
    l
  }

  def nodes = {
    MDD2.timestamp += 1
    root.nodes(MDD2.timestamp)
  }

  def find(f: (Int, Int) => Boolean) = {
    MDD2.timestamp += 1
    root.find(MDD2.timestamp, f, 0) map (_.toArray)
  }

  override def toString = nodes + " nodes representing " + size + " tuples"

  def copy = {
    //println("Copying")s
    MDD2.timestamp += 1
    new MDD2(root.copy(MDD2.timestamp))
  }
  def quickCopy = copy
}

final class MDD2Node(val trie: Array[MDD2Node], var _size: Int) {

  assert(_size == 1 || _size == computeSize)

  def size = {
    assert(_size == computeSize, "%d != %d : %s".format(_size, computeSize, toString))
    _size
  }

  def size_=(v: Int) = {
    assert(this ne MDD2.leaf)
    assert(size == computeSize)
    _size = size
  }

  var copied: MDD2Node = null

  def copy(ts: Int): MDD2Node =
    if (this eq MDD2.leaf) MDD2.leaf
    else if (ts == timestamp) copied
    else {
      timestamp = ts
      copied = new MDD2Node(trie map {
        case null => null
        case t => t.copy(ts)
      }, size)
      copied
    }

  var timestamp = 0

  def depth: Int = 1 + values.map(_.depth).max

  private def values = trie.filter(_ ne null)

  def isEmpty = size == 0

  def +(mdds: HashMap[Seq[MDD2Node], MDD2Node], t: Array[Int]): MDD2Node =
    if (contains(t)) this else this + (mdds, t, 0)

  private def +(mdds: HashMap[Seq[MDD2Node], MDD2Node], tuple: Array[Int], i: Int): MDD2Node =
    if (i >= tuple.length) MDD2.leaf
    else {
      val v = tuple(i)
      val newArray = trie.padTo(v + 1, null)
      //ensureCapacity(v + 1)

      val oldTrie = newArray(v)

      if (oldTrie eq null) {
        newArray(v) = MDD2.empty + (mdds, tuple, i + 1)
      } else {
        newArray(v) = oldTrie + (mdds, tuple, i + 1)
      }
      mdds.getOrElseUpdate(trie, new MDD2Node(trie, size + 1))
    }

  def contains(tuple: Array[Int]): Boolean = contains(tuple, 0)

  @tailrec
  private def contains(tuple: Array[Int], i: Int): Boolean = {
    if (i >= tuple.length) true
    else if (tuple(i) >= trie.length) false
    else trie(tuple(i)) match {
      case null => false
      case t => t.contains(tuple, i + 1)
    }
  }

  //var latest: Option[List[Int]] = None

  def find(ts: Int, f: (Int, Int) => Boolean, depth: Int): Option[List[Int]] = {
    if (this eq MDD2.leaf) Some(Nil)
    else if (timestamp == ts) None
    else {
      timestamp = ts
      var i = trie.length - 1
      while (i >= 0) {
        if ((trie(i) ne null) && f(depth, i)) {
          val found = trie(i).find(ts, f, depth + 1)
          if (found.isDefined) {
            return Some(i :: found.get)
          }
        }

        i -= 1
      }
      None
    }
  }

  override def equals(o: Any): Boolean = o match {
    case t: MDD2Node => trie sameElements t.trie
    case _ => false
  }

  override lazy val hashCode: Int = {
    var result = 1;
    var i = trie.length - 1
    while (i >= 0) {
      result *= 31
      val e = trie(i)
      if (e ne null) {
        result += e.hashCode
      }
      i -= 1
    }
    result;
  }

  override def toString = "MDD2 representing " + _size + " tuples"
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
        if (trie(i) ne null) {
          if (f(depth, i)) l.clear(depth)
          trie(i).fillFound(ts, f, depth + 1, l)
        }
        i -= 1
      }
    }
  }

  //var filteredResult: MDD2Node = null

  def filterTrie(ts: Int, f: (Int, Int) => Boolean, modified: List[Int], depth: Int = 0) {
    if ((this ne MDD2.leaf) && modified.nonEmpty && ts != timestamp) {
      timestamp = ts

      var i = trie.length - 1

      size = 0

      if (modified.head == depth) {
        // Some change at this level
        while (i >= 0) {
          val currentTrie = trie(i)
          if (currentTrie ne null) {
            if (f(depth, i)) {
              currentTrie.filterTrie(ts, f, modified.tail, depth + 1)
              size += currentTrie.size
            } else trie(i) = null
          }
          i -= 1
        }
      } else {
        // No change at this level (=> no need to call f())
        while (i >= 0) {
          val currentTrie = trie(i)
          if (currentTrie ne null) {
            currentTrie.filterTrie(ts, f, modified, depth + 1)
            size += currentTrie.size
          }
          i -= 1
        }
      }
    }
  }

  def listiterator(mdd: MDD2): Iterator[List[Int]] =
    if (this eq MDD2.leaf) Iterator(List())
    else trie.iterator.zipWithIndex flatMap {
      case (t, i) if (t ne null) => t.listiterator(mdd) map (i :: _)
      case _ => Nil
    }

  def nodes(ts: Int): Int = {
    if (ts == timestamp) 0
    else {
      timestamp = ts
      1 + values.map(_.nodes(ts)).sum
    }
  }

  def computeSize: Int = {
    if (this eq MDD2.leaf) 1
    else trie.foldLeft(0) {
      case (acc, null) => acc
      case (acc, t) => acc + t.computeSize
    }
  }

}

