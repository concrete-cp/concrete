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

object Trie extends RelationGenerator {
  def apply(data: Array[Int]*): Trie = apply(data.iterator)

  def apply(data: Iterator[Array[Int]]): Trie = {
    data.foldLeft[Trie](Trie0)(
      (acc, tuple) => acc + tuple).reduce(new HashMap[Seq[Trie], Trie]())
  }

  def newNode(t: Array[Trie]): Trie = {
    var found1I, found2I, found3I: Int = -1
    var found1N, found2N, found3N: Trie = null

    var i = t.length - 1
    while (i >= 0) {
      if (t(i) ne Trie0) {
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
          return new Trien(t)
        }
      }
      i -= 1
    }

    assert(found1I >= 0)
    if (found2I < 0) {
      new Trie1(found1N, found1I)
    } else if (found3I < 0) {
      new Trie2(found2N, found2I, found1N, found1I)
    } else {
      new Trie3(found3N, found3I, found2N, found2I, found1N, found1I)
    }
  }

  def newTrie(i: Int, v: Trie) = {
    val t = Array.fill[Trie](i + 1)(Trie0) //new Array[Trie](i + 1)
    t(i) = v
    t
  }

  def newTrie(t: (Int, Trie)*) = {
    val s: Int = t.map(_._1).max
    val trie = Array.fill[Trie](s + 1)(Trie0)
    addTrie(trie, t: _*)
    trie
  }

  def addTrie(trie: Array[Trie], t: (Int, Trie)*) {
    for ((i, v) <- t) {
      trie(i) = v
    }
  }

}

trait Trie extends Relation {
  type Self2 = Trie

  def +(t: Array[Int]): Trie = if (contains(t)) Trie.this else addTrie(t, 0)
  def addTrie(t: Array[Int], i: Int): Trie
  def reduce(mdds: collection.mutable.Map[Seq[Trie], Trie]): Trie
  def contains(t: Array[Int]): Boolean = contains(t, 0)
  def contains(t: Array[Int], i: Int): Boolean
  def find(f: (Int, Int) => Boolean): Option[Array[Int]] = {
    find(f, 0) map (_.toArray)
  }
  def find(f: (Int, Int) => Boolean, depth: Int): Option[List[Int]]
  def iterator = listIterator.map(_.toArray)
  def listIterator: Iterator[List[Int]]
  def nodes: Int
  def filterTrie(f: (Int, Int) => Boolean, modified: List[Int]): Trie = {
    filterTrie(f, modified, 0)
  }
  def filterTrie(f: (Int, Int) => Boolean, modified: List[Int], depth: Int): Trie
  def fillFound(f: (Int, Int) => Boolean, arity: Int): Traversable[Int] = {
    val l = new ListWithMax(arity)
    fillFound(f, 0, l)
    l
  }
  def fillFound(f: (Int, Int) => Boolean, depth: Int, l: ListWithMax)

  override def toString = s"$nodes nodes representing $size tuples"
  def copy = this
  def -(t: Array[Int]) = throw new UnsupportedOperationException

  def forSubtries(f: (Int, Trie) => Boolean)
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

final object TrieLeaf extends Trie {

  //override def size = 1
  def reduce(mdds: collection.mutable.Map[Seq[Trie], Trie]) = this
  def contains(tuple: Array[Int], i: Int) = true
  def find(f: (Int, Int) => Boolean, depth: Int) = Some(Nil)
  def listIterator = Iterator(Nil)
  def nodes: Int = 1
  def filterTrie(f: (Int, Int) => Boolean, modified: List[Int], depth: Int) = {
    assert(modified.isEmpty)
    this
  }
  def fillFound(f: (Int, Int) => Boolean, depth: Int, l: ListWithMax) = {
    assert(depth > l.max)
  }
  def addTrie(tuple: Array[Int], i: Int) = throw new UnsupportedOperationException
  override def toString = "Trie Leaf"
  def forSubtries(f: (Int, Trie) => Boolean) {
    throw new UnsupportedOperationException
  }
  override def size = 1
  override def isEmpty = false
}

final object Trie0 extends Trie {
  //override def size = 0
  def reduce(mdds: collection.mutable.Map[Seq[Trie], Trie]) = Trie0
  def contains(tuple: Array[Int], i: Int) = false
  def find(f: (Int, Int) => Boolean, depth: Int) = None
  def listIterator = Iterator()
  def nodes = 0
  def filterTrie(f: (Int, Int) => Boolean, modified: List[Int], depth: Int) = Trie0
  def fillFound(f: (Int, Int) => Boolean, depth: Int, l: ListWithMax) {
    throw new UnsupportedOperationException
  }
  def addTrie(tuple: Array[Int], i: Int) = {
    if (i >= tuple.length) {
      TrieLeaf
    } else {
      new Trie1(Trie0.addTrie(tuple, i + 1), tuple(i))
    }
  }
  override def toString = "Empty Trie"
  def forSubtries(f: (Int, Trie) => Boolean) {
    throw new UnsupportedOperationException
  }
  override def isEmpty = true
}

final class Trie1(private val child: Trie, private val index: Int) extends Trie {
  assert(child ne Trie0)

  def forSubtries(f: (Int, Trie) => Boolean) {
    f(index, child)
  }

  def addTrie(t: Array[Int], i: Int): Trie =
    if (i >= t.length) {
      TrieLeaf
    } else {
      val v = t(i)
      if (v == index) {
        new Trie1(child.addTrie(t, i + 1), index)
      } else if (v < index) {
        new Trie2(Trie0.addTrie(t, i + 1), v, child, index)
      } else {
        new Trie2(child, index, Trie0.addTrie(t, i + 1), v)
      }
    }

  def contains(t: Array[Int], i: Int): Boolean = {
    t(i) == index && child.contains(t, i + 1)
  }

  def reduce(mdds: collection.mutable.Map[Seq[Trie], Trie]): Trie = {

    //    } else if (mdds.contains(trie)) {
    //      this
    //    } else

    var b = child.reduce(mdds)
    //
    //    if (b eq EmptyTrie) {
    //      EmptyTrie
    //    } else {
    val newArray = Trie.newTrie(index, b)
    mdds.getOrElseUpdate(newArray, new Trie1(b, index))
    //}

  }

  def find(f: (Int, Int) => Boolean, depth: Int): Option[List[Int]] = {

    if (f(depth, index)) {
      child.find(f, depth + 1).map(index :: _)
    } else {
      None
    }

  }

  override def hashCode: Int = 31 * index + child.hashCode

  override def equals(o: Any): Boolean = o match {
    case t: Trie1 => t.index == index && (t.child eq child)
    case _ => false
  }

  def fillFound(f: (Int, Int) => Boolean, depth: Int, l: ListWithMax) {
    if (depth <= l.max) {
      if (f(depth, index)) l.clear(depth)
      child.fillFound(f, depth + 1, l)
    }
  }

  def filterTrie(f: (Int, Int) => Boolean, modified: List[Int], depth: Int = 0): Trie =
    if (modified.isEmpty) {
      this
    } else {

      //l nC =
      val nC =
        if (modified.head == depth) {
          // Some change at this level
          if (f(depth, index)) {
            child.filterTrie(f, modified.tail, depth + 1)
          } else {
            Trie0
          }
        } else {
          // No change at this level (=> no need to call f())
          child.filterTrie(f, modified, depth + 1)
        }

      if (nC eq Trie0) {
        Trie0
      } else if (nC eq child) {
        this
      } else {
        new Trie1(nC, index)
      }

    }

  def listIterator = child.listIterator.map(index :: _)

  def nodes: Int = 1 + child.nodes
}

final class Trie2(
  private val left: Trie, private val leftI: Int,
  private val right: Trie, private val rightI: Int) extends Trie {
  assert(right ne Trie0)
  assert(left ne Trie0)
  assert(leftI < rightI)

  def forSubtries(f: (Int, Trie) => Boolean) {
    f(leftI, left) && f(rightI, right)
  }

  def addTrie(t: Array[Int], i: Int): Trie =
    if (i >= t.length) {
      TrieLeaf
    } else {
      t(i) match {
        case `leftI` => new Trie2(left.addTrie(t, i + 1), leftI, right, rightI)
        case `rightI` => new Trie2(left, leftI, right.addTrie(t, i + 1), rightI)
        case v: Int if v < leftI =>
          new Trie3(Trie0.addTrie(t, i + 1), v, left, leftI, right, rightI)

        case v: Int if v < rightI =>
          new Trie3(left, leftI, Trie0.addTrie(t, i + 1), v, right, rightI)

        case v: Int =>
          new Trie3(left, leftI, right, rightI, Trie0.addTrie(t, i + 1), v)

      }
    }

  def contains(t: Array[Int], i: Int): Boolean = t(i) match {
    case `leftI` => left.contains(t, i + 1)
    case `rightI` => right.contains(t, i + 1)
    case _ => false
  }

  def fillFound(f: (Int, Int) => Boolean, depth: Int, l: ListWithMax) {

    if (depth <= l.max) {
      if (f(depth, leftI)) l.clear(depth)
      left.fillFound(f, depth + 1, l)
      if (depth <= l.max) {
        if (f(depth, rightI)) l.clear(depth)
        right.fillFound(f, depth + 1, l)
      }
    }

  }

  @inline
  private def filteredTrie(f: (Int, Int) => Boolean, modified: List[Int], depth: Int, t: Trie, i: Int) = {
    if (f(depth, i)) {
      t.filterTrie(f, modified, depth + 1)
    } else {
      Trie0
    }
  }

  def filterTrie(f: (Int, Int) => Boolean, modified: List[Int], depth: Int): Trie =
    if (modified.isEmpty) {
      this
    } else {

      var nL: Trie = null
      var nR: Trie = null

      if (modified.head == depth) {
        // Some change at this level
        nL = filteredTrie(f, modified.tail, depth, left, leftI)
        nR = filteredTrie(f, modified.tail, depth, right, rightI)
      } else {
        // No change at this level (=> no need to call f())
        nL = left.filterTrie(f, modified, depth + 1)
        nR = right.filterTrie(f, modified, depth + 1)
      }

      if (nL eq Trie0) {
        if (nR eq Trie0) {
          Trie0
        } else {
          new Trie1(nR, rightI)
        }
      } else if (nR eq Trie0) {
        new Trie1(nL, leftI)
      } else {
        if ((nL eq left) && (nR eq right)) {
          this
        } else {
          new Trie2(nL, leftI, nR, rightI)
        }
      }

    }

  def find(f: (Int, Int) => Boolean, depth: Int): Option[List[Int]] = {
    check(f, depth, leftI, left).orElse(check(f, depth, rightI, right))
  }

  private def check(f: (Int, Int) => Boolean, depth: Int, direction: Int, trie: Trie): Option[List[Int]] = {
    if (f(depth, direction)) {
      trie.find(f, depth + 1).map(direction :: _)
    } else {
      None
    }
  }

  def listIterator: Iterator[List[Int]] = left.listIterator.map(0 :: _) ++ right.listIterator.map(1 :: _)
  def nodes: Int = 1 + left.nodes + right.nodes

  def reduce(mdds: collection.mutable.Map[Seq[Trie], Trie]): Trie = {
    var bL = left.reduce(mdds)
    var bR = right.reduce(mdds)

    val nT = Trie.newTrie((leftI, bL), (rightI, bR))

    mdds.getOrElseUpdate(nT, new Trie2(bL, leftI, bR, rightI))
  }

  override def hashCode: Int = List(left, leftI, right, rightI).hashCode

  override def equals(o: Any): Boolean = o match {
    case t: Trie2 => (left eq t.left) && (right eq t.right) && (leftI == t.leftI) && (rightI == t.rightI)
    case _ => false
  }

  override def isEmpty = false
}

final class Trie3(
  private val left: Trie, private val leftI: Int,
  private val mid: Trie, private val midI: Int,
  private val right: Trie, private val rightI: Int) extends Trie {
  assert(right ne Trie0)
  assert(mid ne Trie0)
  assert(left ne Trie0)
  assert(leftI < midI)
  assert(midI < rightI)

  def forSubtries(f: (Int, Trie) => Boolean) {
    f(leftI, left) && f(midI, mid) && f(rightI, right)
  }

  def addTrie(t: Array[Int], i: Int): Trie =
    if (i >= t.length) {
      TrieLeaf
    } else {
      t(i) match {
        case `leftI` => new Trie3(left.addTrie(t, i + 1), leftI, mid, midI, right, rightI)
        case `midI` => new Trie3(left, leftI, mid.addTrie(t, i + 1), midI, right, rightI)
        case `rightI` => new Trie3(left, leftI, mid, midI, right.addTrie(t, i + 1), rightI)
        case v: Int => {
          val newArray = Trie.newTrie((leftI, left), (midI, mid), (rightI, right), (v, Trie0.addTrie(t, i + 1)))
          new Trien(newArray)
        }
      }
    }

  def contains(t: Array[Int], i: Int): Boolean = t(i) match {
    case `leftI` => left.contains(t, i + 1)
    case `midI` => mid.contains(t, i + 1)
    case `rightI` => right.contains(t, i + 1)
    case _ => false
  }

  def fillFound(f: (Int, Int) => Boolean, depth: Int, l: ListWithMax) {

    if (depth <= l.max) {
      if (f(depth, leftI)) l.clear(depth)
      left.fillFound(f, depth + 1, l)

      if (depth <= l.max) {
        if (f(depth, midI)) l.clear(depth)
        mid.fillFound(f, depth + 1, l)

        if (depth <= l.max) {
          if (f(depth, rightI)) l.clear(depth)
          right.fillFound(f, depth + 1, l)
        }
      }

    }
  }

  private def filteredTrie(f: (Int, Int) => Boolean, modified: List[Int], depth: Int, t: Trie, i: Int) = {
    if (f(depth, i)) {
      t.filterTrie(f, modified, depth + 1)
    } else {
      Trie0
    }
  }

  private def passedTrie(f: (Int, Int) => Boolean, modified: List[Int], depth: Int, t: Trie) = {
    t.filterTrie(f, modified, depth + 1)
  }

  def filterTrie(f: (Int, Int) => Boolean, modified: List[Int], depth: Int): Trie =
    if (modified.isEmpty) {
      this
    } else {

      var nL: Trie = null
      var nM: Trie = null
      var nR: Trie = null

      if (modified.head == depth) {
        // Some change at this level
        nL = filteredTrie(f, modified.tail, depth, left, leftI)
        nM = filteredTrie(f, modified.tail, depth, mid, midI)
        nR = filteredTrie(f, modified.tail, depth, right, rightI)

      } else {
        // No change at this level (=> no need to call f())
        nL = passedTrie(f, modified, depth, left)
        nM = passedTrie(f, modified, depth, mid)
        nR = passedTrie(f, modified, depth, right)
      }

      if (nL eq Trie0) {
        if (nM eq Trie0) {
          if (nR eq Trie0) {
            Trie0
          } else {
            new Trie1(nR, rightI)
          }
        } else if (nR eq Trie0) {
          new Trie1(nM, midI)
        } else {
          new Trie2(nM, midI, nR, rightI)
        }
      } else if (nM eq Trie0) {
        if (nR eq Trie0) {
          new Trie1(nL, leftI)
        } else {
          new Trie2(nL, leftI, nR, rightI)
        }
      } else {
        if (nR eq Trie0) {
          new Trie2(nL, leftI, nM, midI)
        } else {
          if ((nL eq left) && (nM eq mid) && (nR eq right)) {
            this
          } else {
            new Trie3(nL, leftI, nM, midI, nR, rightI)
          }
        }
      }

    }

  def find(f: (Int, Int) => Boolean, depth: Int): Option[List[Int]] = {

    check(f, depth, leftI, left).orElse(check(f, depth, midI, mid)).orElse(check(f, depth, rightI, right))

  }

  private def check(f: (Int, Int) => Boolean, depth: Int, direction: Int, trie: Trie): Option[List[Int]] = {
    if (f(depth, direction)) {
      trie.find(f, depth + 1).map(direction :: _)
    } else {
      None
    }
  }

  def listIterator: Iterator[List[Int]] = left.listIterator.map(leftI :: _) ++
    mid.listIterator.map(midI :: _) ++
    right.listIterator.map(rightI :: _)

  def nodes: Int = 1 + left.nodes + mid.nodes + right.nodes

  def reduce(mdds: collection.mutable.Map[Seq[Trie], Trie]): Trie = {
    val bL = left.reduce(mdds)
    val bM = mid.reduce(mdds)
    val bR = right.reduce(mdds)

    val nT = Trie.newTrie((leftI, bL), (midI, bM), (rightI, bR))
    mdds.getOrElseUpdate(nT, new Trie3(bL, leftI, bM, midI, bR, rightI))
  }

  override def hashCode: Int = List(left, leftI, mid, midI, right, rightI).hashCode

  override def equals(o: Any): Boolean = o match {
    case t: Trie3 =>
      (left eq t.left) && (right eq t.right) &&
        (mid eq t.mid) && (leftI == t.leftI) &&
        (midI == t.midI) && (rightI == t.rightI)
    case _ => false
  }

  override def isEmpty = false
}

final class Trien(private val trie: Array[Trie]) extends Trie {

  def forSubtries(f: (Int, Trie) => Boolean) {
    forSubtries(f, trie.length - 1)
  }

  @tailrec
  def forSubtries(f: (Int, Trie) => Boolean, i: Int) {
    if (i >= 0) {
      val t = trie(i)
      if ((t eq Trie0) || f(i, t)) {
        forSubtries(f, i - 1)
      }
    }
  }

  def addTrie(tuple: Array[Int], i: Int): Trie = {
    if (i >= tuple.length) {
      TrieLeaf
    } else {
      val v = tuple(i)
      val newTrie = trie.padTo(v + 1, Trie0)
      newTrie(v) = newTrie(v).addTrie(tuple, i + 1)
      new Trien(newTrie)
    }
  }

  def reduce(mdds: collection.mutable.Map[Seq[Trie], Trie]): Trie = {
    var b = trie.map(_.reduce(mdds))
    mdds.getOrElseUpdate(b, new Trien(b))
  }

  //@tailrec
  def contains(tuple: Array[Int], i: Int): Boolean =
    tuple(i) < trie.length && trie(tuple(i)).contains(tuple, i + 1)

  def find(f: (Int, Int) => Boolean, depth: Int): Option[List[Int]] = findHere(f, depth)

  @tailrec
  private def findHere(f: (Int, Int) => Boolean, depth: Int, i: Int = trie.length - 1): Option[List[Int]] = {
    if (i < 0) {
      None
    } else if (f(depth, i)) {
      trie(i).find(f, depth + 1) match {
        case Some(found) => Some(i :: found)
        case None => findHere(f, depth, i - 1)
      }
    } else {
      findHere(f, depth, i - 1)
    }
  }

  override lazy val hashCode: Int = MurmurHash3.arrayHash(trie)

  override def equals(o: Any): Boolean = o match {
    case t: Trien =>
      val len = t.trie.length
      len == trie.length && {
        var i = len - 1
        while (i >= 0 && (t.trie(i) eq trie(i))) i -= 1
        i < 0
      }
    case _ => false
  }

  def fillFound(f: (Int, Int) => Boolean, depth: Int, l: ListWithMax) {
    var i = trie.length - 1
    while (i >= 0 && depth <= l.max) {
      if (trie(i) ne Trie0) {
        if (f(depth, i)) l.clear(depth)
        trie(i).fillFound(f, depth + 1, l)
      }
      i -= 1
    }
  }

  private def udTrie0(trie: Array[Trie], i: Int) {
    if (trie ne null) {
      trie(i) = Trie0
    }
  }

  private def filteredTrie(f: (Int, Int) => Boolean, modified: List[Int], depth: Int) = {

    val trie = this.trie
    var newTrie: Array[Trie] = null
    var i = trie.length - 1
    while (i >= 0) {
      if ((trie(i) ne Trie0) && f(depth, i)) {
        val uT = trie(i).filterTrie(f, modified, depth + 1)
        if (uT ne Trie0) {
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

  private def passedTrie(f: (Int, Int) => Boolean, modified: List[Int], depth: Int): Array[Trie] = {
    val trie = this.trie
    var newTrie: Array[Trie] = null
    var i = trie.length - 1
    while (i >= 0) {
      val nT = trie(i).filterTrie(f, modified, depth)
      if (nT ne Trie0) {
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

  private def fill(s: Int, last: Trie) = {
    val t = new Array[Trie](s)
    var i = s - 1
    t(i) = last
    i -= 1
    while (i >= 0) {
      t(i) = Trie0
      i -= 1
    }
    t
  }

  def filterTrie(f: (Int, Int) => Boolean, modified: List[Int], depth: Int = 0): Trie =
    if (modified.isEmpty) {
      this
    } else {

      val newTrie =
        if (modified.head == depth) {
          // Some change at this level
          filteredTrie(f, modified.tail, depth)
        } else {
          // No change at this level (=> no need to call f())
          passedTrie(f, modified, depth + 1)
        }

      if (newTrie eq null) {
        Trie0
      } else if (same(newTrie, trie)) {
        this
      } else {
        Trie.newNode(newTrie)
      }

    }

  private def same(t1: Array[Trie], t2: Array[Trie]): Boolean = {
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

  def nodes: Int = 1 + trie.map(_.nodes).sum

  override def isEmpty = false

}


