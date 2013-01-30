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
  //        it += 1000
  //      }
  //      acc + (mdds, tuple)
  //    })
  //    mdds.clear()
  //    m
  //    //println(mdds.map(_._2.hashCode).toSeq.sorted)
  //
  //  }

  def apply(data: Iterator[Array[Int]]): MDD = {
    val root = data.foldLeft(empty) {
      (acc, tuple) => acc.addTrie(tuple)
    }
    val reduced = root.reduce(new HashMap[Seq[MDDNode], MDDNode]())
    new MDD(reduced)
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
    new MDD(root.filterTrie(MDD.timestamp, f, modified, 0))
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
}

final class MDDNode(val trie: Array[MDDNode], val size: Int) {

  var timestamp = 0

  def depth: Int = 1 + trie.map(_.depth).max

  def isEmpty = size == 0

  //  def +(mdds: collection.mutable.Map[Seq[MDDNode], MDDNode], t: Array[Int]): MDDNode =
  //    if (contains(t)) this else this + (mdds, t, 0)
  //
  //  private def +(mdds: collection.mutable.Map[Seq[MDDNode], MDDNode], tuple: Array[Int], i: Int): MDDNode =
  //    if (i >= tuple.length) { MDD.leaf }
  //    else {
  //      val v = tuple(i)
  //      val newArray = trie.padTo(v + 1, null)
  //      //ensureCapacity(v + 1)
  //
  //      val oldTrie = newArray(v)
  //
  //      if (oldTrie eq null) {
  //        newArray(v) = MDD.empty + (mdds, tuple, i + 1)
  //      } else {
  //        newArray(v) = oldTrie + (mdds, tuple, i + 1)
  //      }
  //      mdds.getOrElseUpdate(newArray, new MDDNode(newArray, size + 1))
  //    }

  def addTrie(t: Array[Int]): MDDNode = if (contains(t)) this else this.addTrie(t, 0)

  private def addTrie(tuple: Array[Int], i: Int): MDDNode = {
    if (i >= tuple.length) {
      MDD.leaf
    } else {
      val v = tuple(i)
      val newArray = trie.padTo(v + 1, MDD.empty)
      newArray(v) = newArray(v).addTrie(tuple, i + 1)
      new MDDNode(newArray, size + 1)
    }
  }

  def reduce(mdds: collection.mutable.Map[Seq[MDDNode], MDDNode]): MDDNode = {
    if ((this eq MDD.leaf) || (this eq MDD.empty)) {
      this
      //    } else if (mdds.contains(trie)) {
      //      this
//    } else if (trie.forall(m => (m eq MDD.empty)) || trie.forall(m => (m eq MDD.leaf))) {
//      this
    } else {
      var b = trie.map(_.reduce(mdds))

      if (b.forall(_ eq MDD.empty)) {
        MDD.empty
      } else {
        mdds.getOrElseUpdate(b, new MDDNode(b, size))
      }
    }
  }

  def contains(tuple: Array[Int]): Boolean = contains(tuple, 0)

  @tailrec
  private def contains(tuple: Array[Int], i: Int): Boolean = {
    i >= tuple.length || (tuple(i) < trie.length && trie(tuple(i)).contains(tuple, i + 1))
  }

  //  def renew(ts: Int, mdds: collection.mutable.Map[Seq[MDDNode], MDDNode]) {
  //    if (timestamp != ts) {
  //      timestamp = ts
  //      mdds.put(trie, this)
  //      trie.foreach(t => if (t ne null) t.renew(ts, mdds))
  //    }
  //  }

  //var latest: Option[List[Int]] = None

  def find(ts: Int, f: (Int, Int) => Boolean, depth: Int): Option[List[Int]] = {
    if (this eq MDD.leaf) {
      Some(Nil)
    } else if (timestamp == ts) {
      None
    } else {
      timestamp = ts
      var i = trie.length - 1
      while (i >= 0) {
        if (f(depth, i)) {
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
    case t: MDDNode =>
      val len = t.trie.length
      len == trie.length && {
        var i = 0
        while (i < len && (t.trie(i) eq trie(i))) i += 1
        i == len
      }

    case _ => false
  }

  override val hashCode: Int = MurmurHash3.arrayHash(trie)

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
        if (trie(i) ne MDD.empty) {
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
      var newTrie: Array[MDDNode] = new Array[MDDNode](trie.length)
      var i = trie.length - 1
      var newSize = 0

      if (modified.head == depth) {
        // Some change at this level
        while (i >= 0) {
          val currentTrie = trie(i)
          if ((currentTrie eq MDD.empty) || !f(depth, i)) {
            newTrie(i) = MDD.empty
          } else {
            val newSubTrie = currentTrie.filterTrie(ts, f, modified.tail, depth + 1)
            newTrie(i) = newSubTrie
            newSize += newSubTrie.size
          }
          i -= 1
        }
      } else {
        // No change at this level (=> no need to call f())
        while (i >= 0) {
          val currentTrie = trie(i)
          if (currentTrie eq MDD.empty) {
            newTrie(i) = MDD.empty
          } else {
            val newSubTrie = currentTrie.filterTrie(ts, f, modified, depth + 1)
            newTrie(i) = newSubTrie
            newSize += newSubTrie.size
          }
          i -= 1
        }
      }

      filteredResult = if (newSize == 0) {
        MDD.empty
      } else if (size == newSize) {
        this
      } else {
        new MDDNode(newTrie, newSize)
      }

      filteredResult

    }

  def listiterator(mdd: MDD): Iterator[List[Int]] =
    if (this eq MDD.leaf) {
      Iterator(List())
    } else trie.iterator.zipWithIndex flatMap {
      case (t, i) => t.listiterator(mdd) map (i :: _)
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

