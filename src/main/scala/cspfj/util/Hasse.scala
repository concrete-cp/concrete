package cspfj.util
import scala.annotation.tailrec
import scala.collection.immutable.BitSet
import scala.collection.mutable.DoubleLinkedList

object HNode {
  var counts = 0
  var flats = 0
}

class HNode[A](val v: A, var child: UOList[HNode[A]]) {

  def rank: Int = {
    HNode.counts += 1
    count(child, 1)
  }

  def flatten = Hasse.flatten(UOList.empty + this)

  @tailrec
  private def count(s: UOList[HNode[A]], c: Int): Int =
    if (s == Nil) c
    else if (s.head.counted == HNode.counts) count(s.tail, c)
    else {
      s.head.counted = HNode.counts
      //      if (s.head.removed) count(s.head.child ++ s.tail, c)
      //      else 
      count(s.head.child ++ s.tail, c + 1)
    }

  var counted = -1
  var listed = -1
  var collected = -1
  var removed = false

  override def toString = "[" + v + ", " + rank + ", " + child + "]"
}

object Hasse {
  def flatten[A](stack: UOList[HNode[A]]): UOList[HNode[A]] = {
    HNode.flats += 1
    flatten(stack, UOList.empty)
  }

  @tailrec
  private def flatten[A](s: UOList[HNode[A]], f: UOList[HNode[A]]): UOList[HNode[A]] =
    if (s == Nil) f
    else if (s.head.listed == HNode.flats) flatten(s.tail, f)
    else {
      s.head.listed = HNode.flats
      //      if (s.head.removed) flatten(s.head.child ++ s.tail, f)
      //      else 
      flatten(s.head.child ++ s.tail, f + s.head)
    }

}

class Hasse[A](val po: EnhancedPartialOrdering[A]) extends Iterable[(A, Int)] {
  var cleaned = true
  var roots: UOList[HNode[A]] = UOList.empty

  var nodes: Map[A, HNode[A]] = Map.empty

  def add(v: A) {
    clean()
    assert(!nodes.contains(v))
    colls += 1
    val newNode = new HNode(v, collect(v, roots, UOList.empty))
    nodes += v -> newNode

    val supersets = collectParents(v, roots.filter(r => po.lt(v, r.v)), UOList.empty)
    if (supersets.isEmpty) roots = roots.filter(r => !po.lteq(r.v, v)) + newNode
    else supersets.foreach(s =>
      s.child = s.child.filter(r => !po.lteq(r.v, v)) + newNode)
  }

  @tailrec
  private def collectParents(v: A, s: UOList[HNode[A]], collected: UOList[HNode[A]]): UOList[HNode[A]] =
    if (s.isEmpty) collected
    else {
      //      var (removed, remaining) = s.head.child.partition(_.removed)
      //      while (!removed.isEmpty) {
      //
      //        val t = removed.flatMap { n: HNode[A] => n.child }.partition(_.removed)
      //
      //        removed = t._1
      //        remaining ++= t._2
      //
      //      }
      //      s.head.child = remaining
      //      val child = s.head.child.filter(c => po.lt(v, c.v))

      //s.head.child = clean(s.head.child, UOList.empty)

      val child = s.head.child.filter(c => !c.removed && po.lt(v, c.v))

      if (child.isEmpty) collectParents(v, s.tail, collected + s.head)
      else collectParents(v, child ++ s.tail, collected)

    }

  var colls = 0

  //  private def clean(s: UOList[HNode[A]], res: UOList[HNode[A]]): UOList[HNode[A]] = {
  //    if (s.isEmpty) res
  //    else if (s.head.removed) clean(s.tail, res ++ s.head.child)
  //    else clean(s.tail, res + s.head)
  //  }

  @tailrec
  private def collect(v: A, s: UOList[HNode[A]], collected: UOList[HNode[A]]): UOList[HNode[A]] =
    if (s.isEmpty) collected
    else if (s.head.collected == colls) collect(v, s.tail, collected)
    else {
      s.head.collected = colls
      //      if (s.head.removed)
      //        collect(v, s.head.child ++ s.tail, collected)
      //      else 
      if (po.disjoint(s.head.v, v) || collected.exists(c => po.lteq(s.head.v, c.v)))
        collect(v, s.tail, collected)
      else if (po.lteq(s.head.v, v))
        collect(v, s.tail,
          collected.filter(c => !po.lteq(c.v, s.head.v)) + s.head)
      else
        collect(v, s.head.child ++ s.tail, collected)

    }

  override def toString = toGML

  def iterator = Hasse.flatten(roots).iterator.map(n => (n.v, n.rank))

  //  def filter(f: A => Boolean) = new Hasse[A](po, rem(f, roots))
  //

  def remove(v: A) {
    nodes(v).removed = true
    nodes -= v
    cleaned = false
    //roots = rem(v, roots)
  }
  //
  def clean() {
    if (!cleaned) {
      roots = clean(roots)
      cleaned = true
    }
  }

  private def clean(roots: UOList[HNode[A]]): UOList[HNode[A]] = {
    @tailrec
    def remL(roots: UOList[HNode[A]], newRoots: UOList[HNode[A]]): UOList[HNode[A]] =
      if (roots.isEmpty) newRoots
      else {
        val r = roots.head
        if (r.removed) remL(roots.tail, clean(r.child) ++ newRoots)
        else {
          r.child = clean(r.child)
          remL(roots.tail, newRoots + r)
        }
      }

    remL(roots, UOList.empty)
  }
  //
  //  private def rem(v: A, roots: UOList[HNode[A]]): UOList[HNode[A]] = {
  //    @tailrec
  //    def remL(roots: UOList[HNode[A]], newRoots: UOList[HNode[A]]): UOList[HNode[A]] =
  //      if (roots.isEmpty) newRoots
  //      else {
  //        val r = roots.head
  //        if (!po.lteq(v, r.v)) remL(roots.tail, newRoots + r)
  //        else if (r.v == v) remL(roots.tail, r.child ++ newRoots)
  //        else {
  //          r.child = rem(v, r.child)
  //          remL(roots.tail, newRoots + r)
  //        }
  //      }
  //
  //    remL(roots, UOList.empty)
  //
  //  }

  def toGML = {
    val stb = new StringBuilder();
    stb.append("graph [\n");
    stb.append("directed 0\n");

    def flatnodes(r: UOList[HNode[A]]): Set[HNode[A]] =
      r.toSet ++ r.flatMap { v: HNode[A] => flatnodes(v.child) }

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
      if (n.removed) stb.append("graphics [ fill \"#CCCCCC\" ]\n")
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


