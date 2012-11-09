package cspfj.constraint.extension

import java.util.Arrays
import scala.annotation.tailrec
import scala.collection.mutable.HashMap
import scala.collection.mutable.Seq
import cspfj.Statistic

object MDD2 {
  def apply(data: Array[Int]*) = {
    val mdd = data.foldLeft(new MDD2())(_ + _)
    mdd.renew()
    mdd
  }

  val leaf = new MDD2Node(Array(), 1)

  val empty = new MDD2Node(Array(), 0)

  var timestamp = 0

}

final class MDD2(val mdds: HashMap[Seq[MDD2Node], MDD2Node], val root: MDD2Node) extends Relation {

  type Self2 = MDD2

  def this() = this(new HashMap(), MDD2.empty)

  def obtain(trie: Array[MDD2Node], size: Int) = {
    mdds.getOrElseUpdate(trie, new MDD2Node(trie, size))
  }

  def renew() {
    MDD2.timestamp += 1
    mdds.clear()
    root.renew(MDD2.timestamp, mdds)
  }

  override def size = root.size

  def +(t: Array[Int]) = {
    val e = new MDD2(mdds, root + (this, t))
    if (Integer.bitCount(e.size) < 3) e.renew()
    e
  }

  def -(t: Array[Int]) = {
    val e = new MDD2(mdds, root + (this, t))
    if (Integer.bitCount(e.size) < 3) e.renew()
    e
  }

  def filterTrie(f: (Int, Int) => Boolean, modified: List[Int]) = {
    MDD2.timestamp += 1
    val newRoot = root.filterTrie(MDD2.timestamp, f, modified, 0)
    if (newRoot eq null) null
    else new MDD2(mdds, newRoot)

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
    MDD2.timestamp += 1
    new MDD2(new HashMap(), root.copy(MDD2.timestamp))
  }
  def quickCopy = copy
}

final class MDD2Node(val trie: Array[MDD2Node], var size: Int) {

  var copied: MDD2Node = null
  
  def copy(ts: Int): MDD2Node =
    if (isEmpty) this
    else if (ts == timestamp) copied
    else {
      copied = new MDD2Node(trie map (t => if (t eq null) null else t.copy(ts)), size)
      timestamp = ts
      copied
    }

  var timestamp = 0

  def depth: Int = 1 + values.map(_.depth).max

  private def values = trie.filter(_ ne null)

  def isEmpty = size == 0

  def +(mdd: MDD2, t: Array[Int]): MDD2Node = if (contains(t)) this else this + (mdd, t, 0)

  def renew(ts: Int, mdds: HashMap[Seq[MDD2Node], MDD2Node] = new HashMap): HashMap[Seq[MDD2Node], MDD2Node] = {
    if (timestamp != ts) {
      timestamp = ts
      mdds.put(trie, this)
      values.foreach(_.renew(ts, mdds))
    }
    mdds
  }

  private def +(mdd: MDD2, tuple: Array[Int], i: Int): MDD2Node =
    if (i >= tuple.length) MDD2.leaf
    else {
      val v = tuple(i)
      val newArray = trie.padTo(v + 1, null)
      //ensureCapacity(v + 1)

      val oldTrie = newArray(v)

      if (oldTrie eq null) {
        newArray(v) = MDD2.empty + (mdd, tuple, i + 1)
      } else {
        newArray(v) = oldTrie + (mdd, tuple, i + 1)
      }
      mdd.obtain(newArray, size + 1)
    }

  def -(mdd: MDD2, t: Array[Int]): MDD2Node = if (contains(t)) (this - (mdd, t, 0)) else this

  private def -(mdd: MDD2, tuple: Array[Int], i: Int): MDD2Node =
    if (i >= tuple.length) this
    else {
      val v = tuple(i)

      val newArray = trie.clone
      val t = trie(v)

      if (t eq MDD2.leaf) {
        newArray(v) = null
      } else {
        val newTrie = t - (mdd, tuple, i + 1)
        if (newTrie.isEmpty) newArray(v) = null else newArray(v) = newTrie
      }
      mdd.obtain(newArray, size - 1)
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

  override def toString = "MDD2 representing " + size + " tuples"
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

  def filterTrie(ts: Int, f: (Int, Int) => Boolean, modified: List[Int], depth: Int = 0): MDD2Node = {
    if (modified.nonEmpty && ts != timestamp) {
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
    this
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

}

