package cspfj.util
import scala.annotation.tailrec
import scala.collection.immutable.BitSet
import scala.collection.mutable.DoubleLinkedList

object HNode {
  var counts = 0
  var flats = 0
}

class HNode[A](val v: A, var child: List[HNode[A]]) {

  def rank: Int = {
    HNode.counts += 1
    count(child, 1)
  }

  def flatten = Hasse.flatten(this :: Nil)

  @tailrec
  private def count(s: List[HNode[A]], c: Int): Int =
    if (s == Nil) c
    else if (s.head.counted == HNode.counts) count(s.tail, c)
    else {
      s.head.counted = HNode.counts
      count(Hasse.stack(s.head.child, s.tail), c + 1)
    }

  var counted = -1
  var listed = -1
  var collected = -1

  override def toString = "[" + v + ", " + rank + ", " + child + "]"
}

object Hasse {
  def flatten[A](stack: List[HNode[A]]): List[HNode[A]] = {
    HNode.flats += 1
    flatten(stack, Nil)
  }

  @tailrec
  private def flatten[A](s: List[HNode[A]], f: List[HNode[A]]): List[HNode[A]] =
    if (s == Nil) f
    else {
      val head :: tail = s
      if (head.listed == HNode.flats) flatten(tail, f)
      else {
        head.listed = HNode.flats
        flatten(stack(head.child, tail), head :: f)
      }
    }

  /**
   * Stacks n (reversed for efficiency) on s
   */
  @tailrec
  def stack[A](n: List[A], s: List[A]): List[A] =
    if (n == Nil) s
    else stack(n.tail, n.head :: s)
}

class Hasse[A](val po: EnhancedPartialOrdering[A]) extends Iterable[(A, Int)] {

  var roots: List[HNode[A]] = Nil

  def add(v: A) {
    colls += 1
    val newNode = new HNode(v, collect(v, roots, Nil))

    val supersets = collectParents(v, roots.filter(r => po.lt(v, r.v)), Nil)
    if (supersets.isEmpty) roots = newNode :: roots.filter(r => !po.lteq(r.v, v))
    else supersets.foreach(s =>
      s.child = newNode :: s.child.filter(r => !po.lteq(r.v, v)))
  }

  @tailrec
  private def collectParents(v: A, s: List[HNode[A]], collected: List[HNode[A]]): List[HNode[A]] =
    if (s == Nil) collected
    else {
      val head :: tail = s

      val child = head.child.filter(c => po.lt(v, c.v))

      if (child == Nil) collectParents(v, tail, head :: collected)
      else collectParents(v, Hasse.stack(child, tail), collected)

    }

  var colls = 0

  @tailrec
  private def collect(v: A, s: List[HNode[A]], collected: List[HNode[A]]): List[HNode[A]] =
    if (s == Nil) collected
    else {
      val head :: tail = s

      if (head.collected == colls) collect(v, tail, collected)
      else {
        head.collected = colls
        if (po.disjoint(head.v, v) || collected.exists(c => po.lteq(head.v, c.v))) {
          collect(v, tail, collected)
        } else if (po.lteq(head.v, v)) {
          collect(v, tail, head :: (collected.filter(c => !po.lteq(c.v, head.v))))
        } else {
          collect(v, Hasse.stack(head.child, tail), collected)
        }
      }
    }

  override def toString = toGML

  def iterator = Hasse.flatten(roots).iterator.map(n => (n.v, n.rank))

  //  def filter(f: A => Boolean) = new Hasse[A](po, rem(f, roots))
  //

  def remove(v: A) {
    roots = rem(v, roots)
  }

  private def rem(v: A, roots: List[HNode[A]]): List[HNode[A]] = {

    @tailrec
    def remL(roots: List[HNode[A]], newRoots: List[HNode[A]]): List[HNode[A]] =
      if (roots == Nil) newRoots
      else {
        val r :: tail = roots
        if (!po.lteq(v, r.v)) remL(tail, r :: newRoots)
        else if (r.v == v) remL(tail, Hasse.stack(r.child, newRoots))
        else {
          r.child = rem(v, r.child)
          remL(tail, r :: newRoots)
        }
      }

    remL(roots, Nil)

  }

  def toGML = {
    val stb = new StringBuilder();
    stb.append("graph [\n");
    stb.append("directed 0\n");

    def flatnodes(r: List[HNode[A]]): Set[HNode[A]] = r.toSet ++ r.flatMap(v => flatnodes(v.child))

    val edges = new StringBuilder()

    stb.append("node [\n")
    stb.append("id \"root\"\n");
    stb.append("label \"root\"\n")
    stb.append("]\n")

    for (n <- roots) {
      edges.append("edge [\n")
      edges.append("source \"root\"\n")
      edges.append("target \"").append(n.hashCode).append("\"\n")
      edges.append("graphics [ targetArrow \"standard\" ]\n");
      edges.append("]\n")
    }

    for (n <- flatnodes(roots)) {
      stb.append("node [\n");
      stb.append("id \"").append(n.hashCode).append("\"\n");
      stb.append("label \"").append(n.v).append(", ").append(n.rank).append("\"\n");
      stb.append("]\n");

      for (c <- n.child) {
        edges.append("edge [\n");
        edges.append("source \"").append(n.hashCode).append("\"\n");
        edges.append("target \"").append(c.hashCode).append("\"\n");
        edges.append("graphics [ targetArrow \"standard\" ]\n");
        edges.append("]\n");
      }
    }

    stb.append(edges)
    stb.append("]\n").toString
  }

}

trait EnhancedPartialOrdering[A] extends PartialOrdering[A] {
  def disjoint(a: A, b: A): Boolean
}

trait PredefPO[A] extends PartialOrdering[A] {
  def tryCompare(a: A, b: A) = {
    val ab = lteq(a, b)
    val ba = lteq(b, a)

    if (ab && ba) Some(0)
    else if (ab) Some(-1)
    else if (ba) Some(1)
    else None
  }
}


