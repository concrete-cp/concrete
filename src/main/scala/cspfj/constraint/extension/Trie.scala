package cspfj.constraint.extension

import java.util.Arrays
import scala.annotation.tailrec
import scala.collection.mutable.HashMap
import scala.collection.mutable.Seq
import cspfj.Statistic
import scala.collection.mutable.WeakHashMap
import scala.util.hashing.MurmurHash3
import cspfj.util.ListWithMax

object Trie extends RelationGenerator {
  def apply(data: Array[Int]*): Trie = apply(data.iterator)

  def apply(data: Iterator[Array[Int]]): Trie = {
    data.foldLeft[Trie](Trie0) {
      (acc, tuple) => acc + tuple
    }

  }

  def newNode(t: Array[Trie], size: Int): Trie = {
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
          return new Trien(t, size)
        }
      }
      i -= 1
    }

    assert(found1I >= 0)
    if (found2I < 0) {
      new Trie1(found1N, found1I, size)
    } else if (found3I < 0) {
      new Trie2(found2N, found2I, found1N, found1I, size)
    } else {
      new Trie3(found3N, found3I, found2N, found2I, found1N, found1I, size)
    }
  }

}

trait Trie extends Relation {
  type Self2 = Trie

  def +(t: Array[Int]): Trie = if (contains(t)) this else addTrie(t, 0)
  def addTrie(t: Array[Int], i: Int): Trie
  def contains(t: Array[Int]): Boolean = contains(t, 0)
  def contains(t: Array[Int], i: Int): Boolean

  def find(f: (Int, Int) => Boolean) = find(f, 0) map (_.toArray)

  def find(f: (Int, Int) => Boolean, depth: Int): Option[List[Int]]
  def listIterator: Iterator[List[Int]]
  def nodes(): Int

  def filterTrie(f: (Int, Int) => Boolean, modified: List[Int]) = filterTrie(f, modified, 0)

  def filterTrie(f: (Int, Int) => Boolean, modified: List[Int], depth: Int): Trie

  def fillFound(f: (Int, Int) => Boolean, arity: Int) = {
    val l = new ListWithMax(arity)
    fillFound(f, 0, l)
    l
  }
  def fillFound(f: (Int, Int) => Boolean, depth: Int, l: ListWithMax)
  def iterator = listIterator.map(_.toArray)
  def -(t: Array[Int]) = {
    throw new UnsupportedOperationException
  }
  def copy = this
}

final object TrieLeaf extends Trie {
  override def size = 1
  def reduce(mdds: collection.mutable.Map[Seq[Trie], Trie]) = this
  def contains(tuple: Array[Int], i: Int) = true
  def find(f: (Int, Int) => Boolean, depth: Int) = Some(Nil)
  def listIterator = Iterator(Nil)
  def nodes = 1
  def filterTrie(f: (Int, Int) => Boolean, modified: List[Int], depth: Int) = {
    assert(modified.isEmpty)
    this
  }
  def fillFound(f: (Int, Int) => Boolean, depth: Int, l: ListWithMax) = {
    assert(depth > l.max)
  }
  def addTrie(tuple: Array[Int], i: Int) = throw new UnsupportedOperationException
  override def toString = "Trie Leaf"
}

final object Trie0 extends Trie {
  override def size = 0
  def reduce(mdds: collection.mutable.Map[Seq[Trie], Trie]) = Trie0
  def contains(tuple: Array[Int], i: Int) = false
  def find(f: (Int, Int) => Boolean, depth: Int) = None
  def listIterator = Iterator()
  def nodes() = 0
  def filterTrie(f: (Int, Int) => Boolean, modified: List[Int], depth: Int) = Trie0
  def fillFound(f: (Int, Int) => Boolean, depth: Int, l: ListWithMax) {
    throw new UnsupportedOperationException
  }
  def addTrie(tuple: Array[Int], i: Int) = {
    if (i >= tuple.length) {
      TrieLeaf
    } else {
      new Trie1(Trie0.addTrie(tuple, i + 1), tuple(i), 1)
    }
  }
  override def toString = "Empty Trie"
}

final class Trie1(private val child: Trie, private val index: Int, override val size: Int) extends Trie {
  assert(child ne Trie0)

  def addTrie(t: Array[Int], i: Int): Trie =
    if (i >= t.length) {
      TrieLeaf
    } else {
      val v = t(i)
      if (v == index) {
        new Trie1(child.addTrie(t, i + 1), index, size + 1)
      } else {
        new Trie2(child, index, Trie0.addTrie(t, i + 1), v, size + 1)
      }
    }

  def contains(t: Array[Int], i: Int): Boolean = {
    t(i) == index && child.contains(t, i + 1)
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

  override def toString = "Trie representing " + size + " tuples"

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

      var nC: Trie = null

      if (modified.head == depth) {
        // Some change at this level
        if (f(depth, index)) {
          nC = child.filterTrie(f, modified.tail, depth + 1)
        } else {
          nC = Trie0
        }
      } else {
        // No change at this level (=> no need to call f())
        nC = child.filterTrie(f, modified, depth + 1)
      }

      if (nC eq Trie0) {
        Trie0
      } else if (nC eq child) {
        this
      } else {
        new Trie1(nC, index, nC.size)
      }

    }

  def listIterator = child.listIterator.map(index :: _)

  def nodes(): Int = 1 + child.nodes

}

final class Trie2(
  private val left: Trie, private val leftI: Int,
  private val right: Trie, private val rightI: Int,
  override val size: Int) extends Trie {
  assert(right ne Trie0)
  assert(left ne Trie0)
  assert(leftI < rightI)

  def addTrie(t: Array[Int], i: Int): Trie =
    if (i >= t.length) {
      TrieLeaf
    } else {
      t(i) match {
        case `leftI` => new Trie2(left.addTrie(t, i + 1), leftI, right, rightI, size + 1)
        case `rightI` => new Trie2(left, leftI, right.addTrie(t, i + 1), rightI, size + 1)
        case v: Int => {
          val newArray = Array.fill[Trie](math.max(v, rightI) + 1)(Trie0)
          newArray(leftI) = left
          newArray(rightI) = right
          newArray(v) = newArray(v).addTrie(t, i + 1)
          new Trien(newArray, size + 1)
        }
      }
    }

  def contains(t: Array[Int], i: Int): Boolean = t(i) match {
    case `leftI` => left.contains(t, i + 1)
    case `rightI` => right.contains(t, i + 1)
    case _ => false
  }

  def fillFound(f: (Int, Int) => Boolean, depth: Int, l: ListWithMax): Unit =
    {
      if (depth <= l.max) {
        if (f(depth, leftI)) l.clear(depth)
        left.fillFound(f, depth + 1, l)
      }
      if (depth <= l.max) {
        if (f(depth, rightI)) l.clear(depth)
        right.fillFound(f, depth + 1, l)
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
      var nR: Trie = null

      if (modified.head == depth) {
        // Some change at this level
        nL = filteredTrie(f, modified.tail, depth, left, leftI)
        nR = filteredTrie(f, modified.tail, depth, right, rightI)

      } else {
        // No change at this level (=> no need to call f())
        nL = passedTrie(f, modified, depth, left)
        nR = passedTrie(f, modified, depth, right)
      }

      if (nL eq Trie0) {
        if (nR eq Trie0) {
          Trie0
        } else {
          new Trie1(nR, rightI, nR.size)
        }
      } else if (nR eq Trie0) {
        new Trie1(nL, leftI, nL.size)
      } else {
        val s = nL.size + nR.size
        if (s == size) {
          this
        } else {
          new Trie2(nL, leftI, nR, rightI, s)
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
  def nodes(): Int =
    1 + left.nodes + right.nodes

  override def hashCode: Int = {
    MurmurHash3.listHash(List(left, leftI, right, rightI), 0)
  }

  override def equals(o: Any): Boolean = o match {
    case t: Trie2 => (left eq t.left) && (right eq t.right) && (leftI == t.leftI) && (rightI == t.rightI)
    case _ => false
  }
}

final class Trie3(
  private val left: Trie, private val leftI: Int,
  private val mid: Trie, private val midI: Int,
  private val right: Trie, private val rightI: Int,
  override val size: Int) extends Trie {
  assert(right ne Trie0)
  assert(mid ne Trie0)
  assert(left ne Trie0)
  assert(leftI < midI)
  assert(midI < rightI)

  private var timestamp: Int = _
  def addTrie(t: Array[Int], i: Int): Trie =
    if (i >= t.length) {
      TrieLeaf
    } else {
      t(i) match {
        case `leftI` => new Trie3(left.addTrie(t, i + 1), leftI, mid, midI, right, rightI, size + 1)
        case `midI` => new Trie3(left, leftI, mid.addTrie(t, i + 1), midI, right, rightI, size + 1)
        case `rightI` => new Trie3(left, leftI, mid, midI, right.addTrie(t, i + 1), rightI, size + 1)
        case v: Int => {
          val newArray = Array.fill[Trie](math.max(v, rightI) + 1)(Trie0)
          newArray(leftI) = left
          newArray(midI) = mid
          newArray(rightI) = right
          newArray(v) = newArray(v).addTrie(t, i + 1)
          new Trien(newArray, size + 1)
        }
      }
    }

  def contains(t: Array[Int], i: Int): Boolean = t(i) match {
    case `leftI` => left.contains(t, i + 1)
    case `midI` => mid.contains(t, i + 1)
    case `rightI` => right.contains(t, i + 1)
    case _ => false
  }

  def fillFound(f: (Int, Int) => Boolean, depth: Int, l: ListWithMax): Unit =
    {
      if (depth <= l.max) {
        if (f(depth, leftI)) l.clear(depth)
        left.fillFound(f, depth + 1, l)
      }
      if (depth <= l.max) {
        if (f(depth, midI)) l.clear(depth)
        mid.fillFound(f, depth + 1, l)
      }
      if (depth <= l.max) {
        if (f(depth, rightI)) l.clear(depth)
        right.fillFound(f, depth + 1, l)
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
            new Trie1(nR, rightI, nR.size)
          }
        } else if (nR eq Trie0) {
          new Trie1(nM, midI, nM.size)
        } else {
          new Trie2(nM, midI, nR, rightI, nM.size + nR.size)
        }
      } else if (nM eq Trie0) {
        if (nR eq Trie0) {
          new Trie1(nL, leftI, nL.size)
        } else {
          new Trie2(nL, leftI, nR, rightI, nL.size + nR.size)
        }
      } else {
        if (nR eq Trie0) {
          new Trie2(nL, leftI, nM, midI, nL.size + nM.size)
        } else {
          val s = nL.size + nM.size + nR.size
          if (s == size) {
            this
          } else {
            new Trie3(nL, leftI, nM, midI, nR, rightI, s)
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

  def nodes(): Int =
    1 + left.nodes + mid.nodes + right.nodes

  override def hashCode: Int = {
    MurmurHash3.listHash(List(left, leftI, mid, midI, right, rightI), 0)
  }

  override def equals(o: Any): Boolean = o match {
    case t: Trie3 =>
      (left eq t.left) && (right eq t.right) &&
        (mid eq t.mid) && (leftI == t.leftI) &&
        (midI == t.midI) && (rightI == t.rightI)
    case _ => false
  }
}

final class Trien(private val trie: Array[Trie], override val size: Int) extends Trie {
  assert(size > 0)

  def addTrie(tuple: Array[Int], i: Int): Trie = {
    if (i >= tuple.length) {
      TrieLeaf
    } else {
      val v = tuple(i)
      val newArray = trie.padTo(v + 1, Trie0)
      newArray(v) = newArray(v).addTrie(tuple, i + 1)
      new Trien(newArray, size + 1)
    }
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

  override def toString = "Trie representing " + size + " tuples"
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

  private def filteredTrie(f: (Int, Int) => Boolean, modified: List[Int], depth: Int, newTrie: Array[Trie]) = {
    var newSize = 0
    val trie = this.trie
    var i = trie.length - 1
    while (i >= 0) {
      if ((trie(i) ne Trie0) && f(depth, i)) {
        newTrie(i) = trie(i).filterTrie(f, modified, depth + 1)
        newSize += newTrie(i).size
      } else {
        newTrie(i) = Trie0
      }
      i -= 1
    }
    newSize
  }

  private def passedTrie(f: (Int, Int) => Boolean, modified: List[Int], depth: Int, newTrie: Array[Trie]) = {
    var newSize = 0
    val trie = this.trie
    var i = trie.length - 1
    while (i >= 0) {
      newTrie(i) = trie(i).filterTrie(f, modified, depth)
      newSize += newTrie(i).size
      i -= 1
    }
    newSize
  }

  def filterTrie(f: (Int, Int) => Boolean, modified: List[Int], depth: Int = 0): Trie =
    if (modified.isEmpty) {
      this
    } else {

      val newTrie = new Array[Trie](trie.length)
      var newSize = 0

      if (modified.head == depth) {
        // Some change at this level
        newSize = filteredTrie(f, modified.tail, depth, newTrie)
      } else {
        // No change at this level (=> no need to call f())
        newSize = passedTrie(f, modified, depth + 1, newTrie)
      }

      newSize match {
        case 0 => Trie0
        case `size` => this
        case s: Int => Trie.newNode(newTrie, s)
      }

    }

  def listIterator = trie.iterator.zipWithIndex flatMap {
    case (t, i) => t.listIterator map (i :: _)
  }

  def nodes(): Int =
    1 + trie.map(_.nodes).sum

}

