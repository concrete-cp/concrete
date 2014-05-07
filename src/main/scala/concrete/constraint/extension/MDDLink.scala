package concrete.constraint.extension

import scala.annotation.tailrec
import scala.collection.mutable.HashMap
import scala.util.hashing.MurmurHash3
import concrete.priorityqueues.Identified
import concrete.util.SetWithMax
import java.util.Arrays
import concrete.Variable

object MDDLink extends RelationGenerator {
  def apply(data: Seq[Array[Int]]): MDDLink = {
    data.foldLeft[MDDLink](MDDLink0)(
      (acc, tuple) => acc + tuple).reduce(new HashMap())
  }

  var timestamp = 0

}

trait MDDLink extends Relation { //with Identified {

  var timestamp: Int = _

  type Self2 = MDDLink

  def +(t: Array[Int]) = this + t.toList
  def +(t: List[Int]): MDDLink = if (contains(t)) MDDLink.this else add(t)

  def add(t: List[Int]): MDDLink

  def reduce(mdds: collection.mutable.Map[MDDLink, MDDLink]): MDDLink

  def contains(t: Array[Int]) = contains(t.toList)
  def contains(t: List[Int]): Boolean

  def iterator = listIterator.map(_.toArray)

  def listIterator: Iterator[List[Int]]

  def edges: Int = {
    MDDLink.timestamp += 1
    edges(MDDLink.timestamp)
  }

  def edges(ts: Int): Int

  def filterTrie(f: (Int, Int) => Boolean, modified: List[Int]): MDDLink = {
    MDDLink.timestamp += 1
    filterTrie(MDDLink.timestamp, f, modified, 0)
  }

  def filterTrie(ts: Int, f: (Int, Int) => Boolean, modified: List[Int], depth: Int): MDDLink

  def fillFound(f: (Int, Int) => Boolean, arity: Int): Traversable[Int] = {
    MDDLink.timestamp += 1
    val l = new SetWithMax(arity)
    fillFound(MDDLink.timestamp, f, 0, l)
    l
  }

  def fillFound(ts: Int, f: (Int, Int) => Boolean, depth: Int, l: SetWithMax)

  override def toString = s"$edges edges representing $size tuples"

  def -(t: Array[Int]) = throw new UnsupportedOperationException

  def lambda: BigInt = {
    lambda(new HashMap(), this)
  }

  def lambda(map: HashMap[MDDLink, BigInt], mdd: MDDLink): BigInt = {
    if (this eq MDDLinkLeaf) {
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

  def forSubtries(f: (Int, MDDLink) => Boolean)

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

  def arity: Int = {
    if (this eq MDDLinkLeaf) {
      0
    } else {
      var a = 1
      forSubtries {
        (_, t) =>
          a += t.arity
          false
      }
      a
    }
  }

  def copy = this

  def findSupport(scope: Array[Variable], p: Int, i: Int, support: Array[Int]): Option[Array[Int]] = ???
  def find(f: (Int, Int) => Boolean): Option[Array[Int]] = ???
}

final object MDDLinkLeaf extends MDDLink {
  ///override def getId = 0
  //override def size = 1
  def reduce(mdds: collection.mutable.Map[MDDLink, MDDLink]) = this
  def contains(tuple: List[Int]) = true
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
  def add(tuple: List[Int]) = throw new UnsupportedOperationException
  override def toString = "MDDLink Leaf"
  def forSubtries(f: (Int, MDDLink) => Boolean) {
    throw new UnsupportedOperationException
  }

  override def size = 1
  override def isEmpty = false
  def findSupport(ts: Int, scope: Array[Variable], p: Int, i: Int, support: Array[Int], depth: Int) =
    Some(support)

  def copy(ts: Int) = this
}

final object MDDLink0 extends MDDLink {
  //override def getId = throw new UnsupportedOperationException

  def reduce(mdds: collection.mutable.Map[MDDLink, MDDLink]) = this
  def contains(tuple: List[Int]) = false
  def find(ts: Int, f: (Int, Int) => Boolean, depth: Int) = None
  def listIterator = Iterator()
  def edges(ts: Int) = 0
  def filterTrie(ts: Int, f: (Int, Int) => Boolean, modified: List[Int], depth: Int) =
    this
  def fillFound(ts: Int, f: (Int, Int) => Boolean, depth: Int, l: SetWithMax) {
    throw new UnsupportedOperationException
  }
  def add(tuple: List[Int]) = tuple match {
    case Nil => MDDLinkLeaf
    case h :: t => new MDDLinkN(MDDLink0.add(t), MDDLink0, h)
  }

  override def toString = "Empty MDDLink"

  def forSubtries(f: (Int, MDDLink) => Boolean) {}

  override def isEmpty = true

  def findSupport(ts: Int, scope: Array[Variable], p: Int, i: Int, support: Array[Int], depth: Int) =
    None

  def copy(ts: Int) = this
}
//
//final class MDDLink1(private val child: MDDLink, private val index: Int) extends MDDLink {
//  assert(child ne MDDLink0)
//  var timestamp: Int = _
//
//  def forSubtries(f: (Int, MDDLink) => Boolean) {
//    f(index, child)
//  }
//
//  def add(t: List[Int]): MDDLink = t match {
//    case Nil => MDDLinkLeaf
//    case h :: t =>
//      if (h == index) {
//        new MDDLink1(child.add(t), index)
//      } else {
//        new MDDLinkN(MDDLink0.add(t), this, h)
//      }
//  }
//
//  def contains(t: List[Int]): Boolean = {
//    t.head == index && child.contains(t.tail)
//  }
//
//  def reduce(mdds: collection.mutable.Map[MDDLink, MDDLink]): MDDLink = {
//
//    val b = new MDDLink1(child.reduce(mdds), index)
//
//    mdds.getOrElseUpdate(b, b)
//
//  }
//
//  lazy val hashCode = 31 * index + child.hashCode
//
//  def fillFound(ts: Int, f: (Int, Int) => Boolean, depth: Int, l: SetWithMax) {
//    if (timestamp != ts) {
//      timestamp = ts
//      if (depth <= l.max) {
//        if (f(depth, index)) l -= depth
//        if (depth + 1 <= l.max) {
//          child.fillFound(ts, f, depth + 1, l)
//        }
//      }
//    }
//  }
//
//  private var filteredResult: MDDLink = _
//
//  def filterTrie(ts: Int, f: (Int, Int) => Boolean, modified: List[Int], depth: Int = 0): MDDLink =
//    if (modified.isEmpty) {
//      this
//    } else if (ts == timestamp) {
//      filteredResult
//    } else {
//      timestamp = ts
//
//      //l nC =
//      val nC =
//        if (modified.head == depth) {
//          // Some change at this level
//          if (f(depth, index)) {
//            child.filterTrie(ts, f, modified.tail, depth + 1)
//          } else {
//            MDDLink0
//          }
//        } else {
//          // No change at this level (=> no need to call f())
//          child.filterTrie(ts, f, modified, depth + 1)
//        }
//
//      if (nC eq MDDLink0) {
//        filteredResult = MDDLink0
//      } else if (nC eq child) {
//        filteredResult = this
//      } else {
//        filteredResult = new MDDLink1(nC, index)
//      }
//
//      filteredResult
//
//    }
//
//  def listIterator = child.listIterator.map(index :: _)
//
//  def edges(ts: Int): Int = {
//    if (ts == timestamp) {
//      0
//    } else {
//      timestamp = ts
//      1 + child.edges(ts)
//    }
//  }
//
//  override def isEmpty = false
//
//}

final class MDDLinkN(
  private val child: MDDLink,
  private val sibling: MDDLink,
  private val index: Int) extends MDDLink {

  assert(child ne MDDLink0)
  assert(sibling ne MDDLink0)
  assert(sibling ne MDDLinkLeaf)

  //@tailrec
  def forSubtries(f: (Int, MDDLink) => Boolean) {
    if (f(index, child)) {
      sibling.forSubtries(f)
    }
  }

  def add(tuple: List[Int]): MDDLink = tuple match {
    case Nil => MDDLinkLeaf
    case h :: t =>
      if (h == index) {
        new MDDLinkN(child.add(t), sibling, index)
      } else {
        new MDDLinkN(child, sibling.add(tuple), index)
      }

  }

  def reduce(mdds: collection.mutable.Map[MDDLink, MDDLink]): MDDLink = {
    val b = new MDDLinkN(child.reduce(mdds), sibling.reduce(mdds), index)
    mdds.getOrElseUpdate(b, b)
  }

  override lazy val hashCode = 721 * index + 31 * child.hashCode + sibling.hashCode

  override def equals(o: Any): Boolean = o match {
    case n: MDDLinkN => index == n.index && (child eq n.child) && (sibling eq n.sibling)
    case _ => false
  }

  //@tailrec
  def contains(tuple: List[Int]): Boolean = {
    val h :: t = tuple
    (h == index && child.contains(t)) || sibling.contains(tuple)
  }

  //@tailrec
  def fillFound(ts: Int, f: (Int, Int) => Boolean, depth: Int, l: SetWithMax) {
    if (timestamp != ts) {
      timestamp = ts
      if (depth <= l.max) {
        if (f(depth, index)) l -= depth
        child.fillFound(ts, f, depth + 1, l)
        sibling.fillFound(ts, f, depth, l)
      }
    }
  }

  private var filteredResult: MDDLink = _

  def filterTrie(ts: Int, f: (Int, Int) => Boolean, modified: List[Int], depth: Int = 0): MDDLink =
    if (modified.isEmpty) {
      this
    } else if (ts == timestamp) {
      filteredResult
    } else {
      timestamp = ts

      val newSibling = sibling.filterTrie(ts, f, modified, depth)
      if (modified.head == depth) {
        // Some change
        if (f(depth, index)) {
          new MDDLinkN(child.filterTrie(ts, f, modified.tail, depth + 1), newSibling, index)
        } else {
          newSibling
        }
      } else {
        new MDDLinkN(child.filterTrie(ts, f, modified, depth + 1), newSibling, index)
      }

      //      filteredResult =
      //        if (modified.head == depth) {
      //          // Some change at this level
      //          if (f(depth, index)) {
      //            val newChild = child.filterTrie(ts, f, modified.tail, depth + 1)
      //
      //               if (newChild.isEmpty) {
      //                newSibling
      //              } else {
      //                new MDDLinkN(newChild, index)
      //              }
      //            } else {
      //              if (newChild.isEmpty) {
      //                newSibling
      //              } else if ((newChild eq child) && (newSibling eq sibling)) {
      //                this
      //              } else {
      //                new MDDLinkN(newChild, newSibling, index)
      //              }
      //            }
      //          } else {
      //            newSibling
      //          }
      //        } else {
      //
      //        }
      //
      //      filteredResult

    }

  def listIterator = child.listIterator.map(index :: _) ++ sibling.listIterator

  def edges(ts: Int): Int = {
    if (ts == timestamp) {
      0
    } else {
      timestamp = ts
      1 + child.edges(ts) + sibling.edges(ts)
      //nbIndices + indices.take(nbIndices).map(trie(_)).map(_.edges(ts)).sum
    }
  }

  override def isEmpty = false

  //  def findSupport(ts: Int, f: (Int, Int) => Boolean, p: Int, i: Int, support: Array[Int], depth: Int) = {
  //    if (timestamp == ts) {
  //      None
  //    } else {
  //      timestamp = ts
  //
  //      if (depth == p) {
  //        if (i >= trie.length) {
  //          None
  //        } else {
  //          support(depth) = i
  //          trie(i).findSupport(ts, f, p, i, support, depth + 1)
  //        }
  //      } else {
  //        var s: Option[Array[Int]] = None
  //        var j = nbIndices - 1
  //        while (s.isEmpty && j >= 0) {
  //          val index = indices(j)
  //          support(depth) = index
  //          s = trie(index).findSupport(ts, f, p, i, support, depth + 1)
  //          j -= 1
  //        }
  //        s
  //      }
  //    }
  //  }
}


