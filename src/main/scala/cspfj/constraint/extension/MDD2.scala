package cspfj.constraint.extension

import scala.annotation.tailrec
import scala.collection.mutable.HashMap
import scala.collection.mutable.Seq
import scala.util.hashing.MurmurHash3
import cspfj.util.ListWithMax
import java.util.Arrays
import cspfj.util.SparseMap

object MDDSparse extends RelationGenerator {
  def apply(data: Array[Int]*): MDD = apply(data.iterator)

  def apply(data: Iterator[Array[Int]]): MDD =
    data.foldLeft[MDD](MDD20)(
      (acc, tuple) => acc + tuple).reduce(new HashMap[Seq[MDD], MDD]())

  var timestamp = 0

  def newTrie(i: Int, v: MDD) = {
    val t = Array.fill[MDD](i + 1)(MDD20)
    t(i) = v
    t
  }

  def newTrie(t: (Int, MDD)*) = {
    val s: Int = t.map(_._1).max
    val trie = Array.fill[MDD](s + 1)(MDD20)
    addTrie(trie, t: _*)
    trie
  }

  def addTrie(trie: Array[MDD], t: (Int, MDD)*) {
    for ((i, v) <- t) {
      trie(i) = v
    }
  }

  def newNode(trie: SparseMap[MDD], s: Int) = new MDD2n(trie, s)

}

final object MDD20 extends MDD {
  override def size = 0
  def reduce(mdds: collection.mutable.Map[Seq[MDD], MDD]) = MDD20
  def contains(tuple: Array[Int], i: Int) = false
  def find(ts: Int, f: (Int, Int) => Boolean, depth: Int) = None
  def listIterator = Iterator()
  def nodes(ts: Int) = 0
  def filterTrie(ts: Int, f: (Int, Int) => Boolean, modified: List[Int], depth: Int) = MDD20
  def fillFound(ts: Int, f: (Int, Int) => Boolean, depth: Int, l: ListWithMax) {
    throw new UnsupportedOperationException
  }
  def addTrie(tuple: Array[Int], i: Int) = {
    if (i >= tuple.length) {
      MDDLeaf
    } else {
      new MDD21(MDD20.addTrie(tuple, i + 1), tuple(i), 1)
    }
  }
  override def toString = "Empty MDD"
}

final class MDD21(private val child: MDD, private val index: Int, override val size: Int) extends MDD {
  assert(child ne MDD20)
  private var timestamp: Int = _
  def addTrie(t: Array[Int], i: Int): MDD =
    if (i >= t.length) {
      MDDLeaf
    } else {
      val v = t(i)
      if (v == index) {
        new MDD21(child.addTrie(t, i + 1), index, size + 1)
      } else if (v < index) {
        new MDD22(MDD20.addTrie(t, i + 1), v, child, index, size + 1)
      } else {
        new MDD22(child, index, MDD20.addTrie(t, i + 1), v, size + 1)
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
    val newArray = MDDSparse.newTrie((index, b))
    mdds.getOrElseUpdate(newArray, new MDD21(b, index, size))
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
    case t: MDD21 => t.index == index && (t.child eq child)
    case _ => false
  }

  def fillFound(ts: Int, f: (Int, Int) => Boolean, depth: Int, l: ListWithMax) {
    if (timestamp != ts) {
      timestamp = ts
      if (depth <= l.max) {
        if (f(depth, index)) l.clear(depth)
        child.fillFound(ts, f, depth + 1, l)
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
            MDD20
          }
        } else {
          // No change at this level (=> no need to call f())
          child.filterTrie(ts, f, modified, depth + 1)
        }

      if (nC eq MDD20) {
        filteredResult = MDD20
      } else if (nC eq child) {
        filteredResult = this
      } else {
        filteredResult = new MDD21(nC, index, nC.size)
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

final class MDD22(
  private val left: MDD, private val leftI: Int,
  private val right: MDD, private val rightI: Int,
  override val size: Int) extends MDD {
  assert(right ne MDD20)
  assert(left ne MDD20)
  assert(leftI < rightI)

  private var timestamp: Int = _
  def addTrie(t: Array[Int], i: Int): MDD =
    if (i >= t.length) {
      MDDLeaf
    } else {
      t(i) match {
        case `leftI` => new MDD22(left.addTrie(t, i + 1), leftI, right, rightI, size + 1)
        case `rightI` => new MDD22(left, leftI, right.addTrie(t, i + 1), rightI, size + 1)
        case v: Int if v < leftI =>
          new MDD23(MDD20.addTrie(t, i + 1), v, left, leftI, right, rightI, size + 1)

        case v: Int if v < rightI =>
          new MDD23(left, leftI, MDD20.addTrie(t, i + 1), v, right, rightI, size + 1)

        case v: Int =>
          new MDD23(left, leftI, right, rightI, MDD20.addTrie(t, i + 1), v, size + 1)

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
        left.fillFound(ts, f, depth + 1, l)
        if (depth <= l.max) {
          if (f(depth, rightI)) l.clear(depth)
          right.fillFound(ts, f, depth + 1, l)
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
      MDD20
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
        if (nL eq MDD20) {
          if (nR eq MDD20) {
            MDD20
          } else {
            new MDD21(nR, rightI, nR.size)
          }
        } else if (nR eq MDD20) {
          new MDD21(nL, leftI, nL.size)
        } else {
          val s = nL.size + nR.size
          if (s == size) {
            this
          } else {
            new MDD22(nL, leftI, nR, rightI, s)
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
    1 + left.nodes(ts) + right.nodes(ts)
  }

  def reduce(mdds: collection.mutable.Map[Seq[MDD], MDD]): MDD = {
    var bL = left.reduce(mdds)
    var bR = right.reduce(mdds)

    val nT = MDDSparse.newTrie((leftI, bL), (rightI, bR))

    mdds.getOrElseUpdate(nT, new MDD22(bL, leftI, bR, rightI, size))
  }

  override def hashCode: Int = List(left, leftI, right, rightI).hashCode

  override def equals(o: Any): Boolean = o match {
    case t: MDD22 => (left eq t.left) && (right eq t.right) && (leftI == t.leftI) && (rightI == t.rightI)
    case _ => false
  }
}

final class MDD23(
  private val left: MDD, private val leftI: Int,
  private val mid: MDD, private val midI: Int,
  private val right: MDD, private val rightI: Int,
  override val size: Int) extends MDD {
  assert(right ne MDD20)
  assert(mid ne MDD20)
  assert(left ne MDD20)
  assert(leftI < midI)
  assert(midI < rightI)

  private var timestamp: Int = _
  def addTrie(t: Array[Int], i: Int): MDD =
    if (i >= t.length) {
      MDDLeaf
    } else {
      t(i) match {
        case `leftI` => new MDD23(left.addTrie(t, i + 1), leftI, mid, midI, right, rightI, size + 1)
        case `midI` => new MDD23(left, leftI, mid.addTrie(t, i + 1), midI, right, rightI, size + 1)
        case `rightI` => new MDD23(left, leftI, mid, midI, right.addTrie(t, i + 1), rightI, size + 1)
        case v: Int => {
          var sm = new SparseMap[MDD]()
          sm += leftI -> left
          sm += midI -> mid
          sm += rightI -> right
          sm += v -> MDD20.addTrie(t, i + 1)
          new MDD2n(sm, size + 1)
        }
      }
    }

  def contains(t: Array[Int], i: Int): Boolean = t(i) match {
    case `leftI` => left.contains(t, i + 1)
    case `midI` => mid.contains(t, i + 1)
    case `rightI` => right.contains(t, i + 1)
    case _ => false
  }

  def fillFound(ts: Int, f: (Int, Int) => Boolean, depth: Int, l: ListWithMax) {
    if (timestamp != ts) {
      timestamp = ts
      if (depth <= l.max) {
        if (f(depth, leftI)) l.clear(depth)
        left.fillFound(ts, f, depth + 1, l)

        if (depth <= l.max) {
          if (f(depth, midI)) l.clear(depth)
          mid.fillFound(ts, f, depth + 1, l)

          if (depth <= l.max) {
            if (f(depth, rightI)) l.clear(depth)
            right.fillFound(ts, f, depth + 1, l)
          }
        }
      }
    }
  }

  private var filteredResult: MDD = _

  private def filteredTrie(ts: Int, f: (Int, Int) => Boolean, modified: List[Int], depth: Int, t: MDD, i: Int) = {
    if (f(depth, i)) {
      t.filterTrie(ts, f, modified, depth + 1)
    } else {
      MDD20
    }
  }

  private def passedTrie(ts: Int, f: (Int, Int) => Boolean, modified: List[Int], depth: Int, t: MDD) = {
    t.filterTrie(ts, f, modified, depth + 1)
  }

  def filterTrie(ts: Int, f: (Int, Int) => Boolean, modified: List[Int], depth: Int): MDD =
    if (modified.isEmpty) {
      this
    } else if (ts == timestamp) {
      filteredResult
    } else {
      timestamp = ts

      var nL: MDD = null
      var nM: MDD = null
      var nR: MDD = null

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
        if (nL eq MDD20) {
          if (nM eq MDD20) {
            if (nR eq MDD20) {
              MDD20
            } else {
              new MDD21(nR, rightI, nR.size)
            }
          } else if (nR eq MDD20) {
            new MDD21(nM, midI, nM.size)
          } else {
            new MDD22(nM, midI, nR, rightI, nM.size + nR.size)
          }
        } else if (nM eq MDD20) {
          if (nR eq MDD20) {
            new MDD21(nL, leftI, nL.size)
          } else {
            new MDD22(nL, leftI, nR, rightI, nL.size + nR.size)
          }
        } else {
          if (nR eq MDD20) {
            new MDD22(nL, leftI, nM, midI, nL.size + nM.size)
          } else {
            val s = nL.size + nM.size + nR.size
            if (s == size) {
              this
            } else {
              new MDD23(nL, leftI, nM, midI, nR, rightI, s)
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

  private def check(ts: Int, f: (Int, Int) => Boolean, depth: Int, direction: Int, trie: MDD): Option[List[Int]] = {
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

  def reduce(mdds: collection.mutable.Map[Seq[MDD], MDD]): MDD = {
    val bL = left.reduce(mdds)
    val bM = mid.reduce(mdds)
    val bR = right.reduce(mdds)

    val nT = MDDSparse.newTrie((leftI, bL), (midI, bM), (rightI, bR))
    mdds.getOrElseUpdate(nT, new MDD23(bL, leftI, bM, midI, bR, rightI, size))
  }

  override def hashCode: Int = List(left, leftI, mid, midI, right, rightI).hashCode

  override def equals(o: Any): Boolean = o match {
    case t: MDD23 =>
      (left eq t.left) && (right eq t.right) &&
        (mid eq t.mid) && (leftI == t.leftI) &&
        (midI == t.midI) && (rightI == t.rightI)
    case _ => false
  }
}

final class MDD2n(private val trie: SparseMap[MDD], override val size: Int) extends MDD {
  assert(size > 0)
  private var timestamp = 0

  def addTrie(tuple: Array[Int], i: Int): MDD = {
    if (i >= tuple.length) {
      MDDLeaf
    } else {
      val v = tuple(i)
      val newTrie = trie + (v -> trie.get(v).getOrElse(MDD20).addTrie(tuple, i + 1))
      new MDD2n(newTrie, size + 1)
    }
  }

  def reduce(mdds: collection.mutable.Map[Seq[MDD], MDD]): MDD = {
    val nt = trie.mapValues(_.reduce(mdds))
    val b = MDDSparse.newTrie(nt.toSeq: _*)
    mdds.getOrElseUpdate(b, new MDD2n(nt, size))
  }

  //@tailrec
  def contains(tuple: Array[Int], i: Int): Boolean =
    trie.get(tuple(i)).map(_.contains(tuple, i + 1)).getOrElse(false)

  def find(ts: Int, f: (Int, Int) => Boolean, depth: Int): Option[List[Int]] = {
    if (timestamp == ts) {
      None
    } else {
      timestamp = ts
      findHere(ts, f, depth)
    }
  }

  private def findHere(ts: Int, f: (Int, Int) => Boolean, depth: Int): Option[List[Int]] = {
    val it = trie.iterator
    while (it.hasNext) {
      val (k, v) = it.next
      val found = v.find(ts, f, depth + 1)
      if (found.isDefined) {
        return Some(k :: found.get)
      }
    }
    None
  }

  override lazy val hashCode: Int = trie.hashCode

  override def equals(o: Any): Boolean = o match {
    case t: MDD2n =>
      t.trie.size == trie.size && t.trie.forall(t => trie.get(t._1).map(_ eq t._2).getOrElse(false))

    case _ => false
  }

  def fillFound(ts: Int, f: (Int, Int) => Boolean, depth: Int, l: ListWithMax) {
    if (timestamp != ts) {
      timestamp = ts
      val it = trie.iterator
      while (it.hasNext && depth <= l.max) {
        val (k, v) = it.next()
        if (v ne MDD20) {
          if (f(depth, k)) l.clear(depth)
          v.fillFound(ts, f, depth + 1, l)
        }
      }
    }
  }

  private var filteredResult: MDD = _

  private def filteredTrie(ts: Int, f: (Int, Int) => Boolean, modified: List[Int], depth: Int) = {

    trie.filter { i: Int =>
      f(depth, i)
    } mapValues {
      _.filterTrie(ts, f, modified, depth + 1)
    }

  }

  private def passedTrie(ts: Int, f: (Int, Int) => Boolean, modified: List[Int], depth: Int) = {
    trie.mapValues {
      _.filterTrie(ts, f, modified, depth + 1)
    }
  }

  def filterTrie(ts: Int, f: (Int, Int) => Boolean, modified: List[Int], depth: Int = 0): MDD =
    if (modified.isEmpty) {
      this
    } else if (ts == timestamp) {
      filteredResult
    } else {
      timestamp = ts

      val newTrie =
        if (modified.head == depth) {
          // Some change at this level
          filteredTrie(ts, f, modified.tail, depth)
        } else {
          // No change at this level (=> no need to call f())
          passedTrie(ts, f, modified, depth + 1)
        }

      val newSize = newTrie.foldLeft(0)(_ + _._2.size)

      filteredResult = newSize match {
        case 0 => MDD20
        case `size` => this
        case s: Int => MDDSparse.newNode(newTrie, s)
      }
      filteredResult

    }

  def listIterator = trie.iterator flatMap {
    case (i, t) => t.listIterator map (i :: _)
  }

  def nodes(ts: Int): Int = {
    if (ts == timestamp) {
      0
    } else {
      timestamp = ts
      1 + trie.map(_._2.nodes(ts)).sum
    }
  }

}


