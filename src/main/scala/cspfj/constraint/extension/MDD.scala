package cspfj.constraint.extension

import java.util.Arrays
import scala.annotation.tailrec
import scala.collection.mutable.HashMap
import scala.collection.mutable.Seq
import cspfj.Statistic

object MDD {
  def apply(data: Array[Int]*) = {
    val mdds = new HashMap[Seq[MDDNode], MDDNode]()
    new MDD(data.foldLeft(empty)(_ + (mdds, _)))
  }

  val leaf = new MDDNode(Array(), 1)

  val empty = new MDDNode(Array(), 0)

  var timestamp = 0

}

final class MDD(val root: MDDNode) extends Relation {

  type Self2 = MDD

  def this() = this(MDD.empty)

  override def size = root.size

  def +(t: Array[Int]) = {
    throw new UnsupportedOperationException
  }

  def -(t: Array[Int]) = {
    throw new UnsupportedOperationException
  }

  def filterTrie(f: (Int, Int) => Boolean, modified: List[Int]) = {
    MDD.timestamp += 1
    val newRoot = root.filterTrie(MDD.timestamp, f, modified, 0)
    if (newRoot eq null) null
    else new MDD(newRoot)

  }

  def contains(t: Array[Int]) = root.contains(t)

  def iterator = root.listiterator(this).map(_.toArray)

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
  def quickCopy = this
}

final class MDDNode(val trie: Array[MDDNode], val size: Int) {

  var timestamp = 0

  def depth: Int = 1 + values.map(_.depth).max

  private def values = trie.filter(_ ne null)

  def isEmpty = size == 0

  def +(mdds: HashMap[Seq[MDDNode], MDDNode], t: Array[Int]): MDDNode =
    if (contains(t)) this else this + (mdds, t, 0)

  private def +(mdds: HashMap[Seq[MDDNode], MDDNode], tuple: Array[Int], i: Int): MDDNode =
    if (i >= tuple.length) MDD.leaf
    else {
      val v = tuple(i)
      val newArray = trie.padTo(v + 1, null)
      //ensureCapacity(v + 1)

      val oldTrie = newArray(v)

      if (oldTrie eq null) {
        newArray(v) = MDD.empty + (mdds, tuple, i + 1)
      } else {
        newArray(v) = oldTrie + (mdds, tuple, i + 1)
      }
      mdds.getOrElseUpdate(newArray, new MDDNode(newArray, size + 1))
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
    if (this eq MDD.leaf) Some(Nil)
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
    case t: MDDNode => trie sameElements t.trie
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
        if (trie(i) ne null) {
          if (f(depth, i)) l.clear(depth)
          trie(i).fillFound(ts, f, depth + 1, l)
        }
        i -= 1
      }
    }
  }

  var filteredResult: MDDNode = null

  def filterTrie(ts: Int, f: (Int, Int) => Boolean, modified: List[Int], depth: Int = 0): MDDNode =
    if (modified.isEmpty) this
    else if (ts == timestamp) filteredResult
    else {
      timestamp = ts
      var newTrie: Array[MDDNode] = null
      var i = trie.length - 1
      var newSize = 0

      assert(trie(i) ne null)

      if (modified.head == depth) {
        // Some change at this level
        while (i >= 0) {
          val currentTrie = trie(i)
          if ((currentTrie ne null) && f(depth, i)) {

            val newSubTrie = currentTrie.filterTrie(ts, f, modified.tail, depth + 1)
            if (newSubTrie ne null) {
              newTrie(i) = newSubTrie
              newSize += newSubTrie.size
            }
          }
          i -= 1
        }
      } else {
        // No change at this level (=> no need to call f())
        while (i >= 0) {
          val currentTrie = trie(i)
          if (currentTrie ne null) {
            val newSubTrie = currentTrie.filterTrie(ts, f, modified, depth + 1)
            if (newSubTrie ne null) {
              if (newTrie eq null) {
                newTrie = new Array[MDDNode](i + 1)
              }
              newTrie(i) = newSubTrie
              newSize += newSubTrie.size
            }
          }
          i -= 1
        }
      }

      filteredResult = if (newSize == 0) null
      else if (size == newSize) this
      else new MDDNode(newTrie, newSize)

      filteredResult

    }

  def listiterator(mdd: MDD): Iterator[List[Int]] =
    if (this eq MDD.leaf) Iterator(List())
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

}

