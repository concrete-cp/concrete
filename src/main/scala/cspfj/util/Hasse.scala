package cspfj.util
import scala.annotation.tailrec

object HNode {
  var id = 0
}

final case class HNode[A](
  val v: A,
  val child: List[HNode[A]]) {
  val rank: Int = 1 + count(child, Set.empty)
  val id = HNode.id
  HNode.id += 1

  @tailrec
  private def count(stack: List[HNode[A]], c: Set[HNode[A]]): Int = {
    if (stack == Nil) c.size
    else {
      val head :: rem = stack
      if (c(head)) count(rem, c)
      else count(head.child ::: rem, c + head)
    }
  }
}

object Hasse {
  def empty[A](po: EnhancedPartialOrdering[A]) = new Hasse[A](po, Nil)
}

final class Hasse[A](val po: EnhancedPartialOrdering[A], val roots: List[HNode[A]]) {

  def +(v: A): Hasse[A] = new Hasse[A](po, add(v, roots))

  private def add(v: A, roots: List[HNode[A]]): List[HNode[A]] = {

    val newNode = HNode(v, collect(v, roots, Nil))

    attach(newNode, roots)
  }

  private def attach(n: HNode[A], roots: List[HNode[A]]): List[HNode[A]] = {
    if (roots.contains(n)) roots
    else {
      val (supersets, remaining) = roots.partition { r =>
        // Strict superset to avoid creating cycles
        po.tryCompare(n.v, r.v) == Some(-1)
      }
      if (supersets.isEmpty) n :: remaining.filter(r => !po.lteq(r.v, n.v))
      else {
        supersets.map(s => HNode(s.v, attach(n, s.child))) ::: remaining
      }
    }
  }

  @tailrec
  private def collect(v: A, stack: List[HNode[A]], collected: List[HNode[A]]): List[HNode[A]] =
    if (stack == Nil) collected
    else {
      val head :: tail = stack

      if (collected.exists(c => po.lteq(head.v, c.v))) collect(v, tail, collected)
      else if (po.lteq(head.v, v)) collect(v, tail, head :: collected)
      else collect(v, head.child ::: tail, collected)
    }

  override def toString: String = toGML

  def toGML = {
    val stb = new StringBuilder();
    stb.append("graph [\n");
    stb.append("directed 0\n");

    def flatnodes(r: List[HNode[A]]): Set[HNode[A]] = r.toSet ++ r.flatMap(v => flatnodes(v.child))

    val edges = new StringBuilder()

    for (n <- flatnodes(roots)) {
      stb.append("node [\n");
      stb.append("id \"").append(n.id).append("\"\n");
      stb.append("label \"").append(n.v).append(", ").append(n.rank).append("\"\n");
      stb.append("]\n");

      for (c <- n.child) {
        edges.append("edge [\n");
        edges.append("source \"").append(n.id).append("\"\n");
        edges.append("target \"").append(c.id).append("\"\n");
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

class SetInclusion[A] extends PredefPO[Set[A]] with EnhancedPartialOrdering[Set[A]] {
  def lteq(a: Set[A], b: Set[A]) = a.subsetOf(b)

  def disjoint(a: Set[A], b: Set[A]) = {
    require(!a.isEmpty)
    require(!b.isEmpty)
    (a & b).isEmpty
  }
}

