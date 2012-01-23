package cspfj.util
import scala.annotation.tailrec

object HNode {
  var id = 0
}

class HNode[A](val v: A, var child: List[HNode[A]]) {
  override val hashCode = HNode.id
  HNode.id += 1

  def rank: Int = stream.size

  def stream = Hasse.stream(this :: Nil)

  override def toString = "[" + v + " (" + hashCode + "), " + rank + ", " + child + "]"
}

object Hasse {
  def stream[A](stack: List[HNode[A]]): Stream[HNode[A]] = stream(stack, Set[HNode[A]]())

  private def stream[A](stack: List[HNode[A]], done: Set[HNode[A]]): Stream[HNode[A]] =
    if (stack == Nil) Stream.empty
    else {
      val head :: tail = stack
      if (done(head)) stream(tail, done)
      else head #:: stream(head.child ::: tail, done + head)
    }
}

class Hasse[A](val po: EnhancedPartialOrdering[A]) extends Iterable[(A, Int)] {

  var roots: List[HNode[A]] = Nil

  //var nodes: Map[A, List[HNode[A]]] = Map.empty.withDefaultValue(Nil)

  def add(v: A) = {
    val newNode = new HNode(v, collect(v, roots, Nil))
    //nodes += v -> (newNode :: nodes(v))
    val supersets = collectParents(v, roots.filter(c => po.lt(v, c.v)), Nil)
    if (supersets.isEmpty) roots ::= newNode
    else supersets.foreach(s =>
      s.child = newNode :: s.child.filter(r => !po.lteq(r.v, newNode.v)))
  }

  @tailrec
  private def collectParents(v: A, stack: List[HNode[A]], collected: List[HNode[A]]): List[HNode[A]] =
    if (stack == Nil) collected
    else {
      val head :: tail = stack

      val child = head.child.filter(c => po.lt(v, c.v))

      if (child == Nil) collectParents(v, tail, head :: collected)
      else collectParents(v, child ::: tail, collected)

    }

  @tailrec
  private def collect(v: A, stack: List[HNode[A]], collected: List[HNode[A]]): List[HNode[A]] =
    if (stack == Nil) collected
    else {
      val head :: tail = stack

      if (po.disjoint(head.v, v) || collected.exists(c => po.lteq(head.v, c.v))) collect(v, tail, collected)
      else if (po.lteq(head.v, v)) collect(v, tail, head :: (collected.filter(c => !po.lteq(c.v, head.v))))
      else collect(v, head.child ::: tail, collected)
    }

  override def toString = iterator.mkString(", ")

  def iterator = Hasse.stream(roots).iterator.map(n => (n.v, n.rank))

  //  def filter(f: A => Boolean) = new Hasse[A](po, rem(f, roots))
  //

  def remove(v: A) { roots = rem(v, roots) }

  private def rem(v: A, roots: List[HNode[A]]): List[HNode[A]] = {
    roots flatMap { r =>
      if (!po.lteq(v, r.v)) List(r)
      if (r.v == v) r.child
      else {
        r.child = rem(v, r.child)
        List(r)
      }
    }
  }

  def toGML = {
    val stb = new StringBuilder();
    stb.append("graph [\n");
    stb.append("directed 0\n");

    def flatnodes(r: List[HNode[A]]): Set[HNode[A]] = r.toSet ++ r.flatMap(v => flatnodes(v.child))

    val edges = new StringBuilder()

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

class SetInclusion[A] extends PredefPO[Set[A]] with EnhancedPartialOrdering[Set[A]] {
  def lteq(a: Set[A], b: Set[A]) = a.subsetOf(b)

  override def lt(a: Set[A], b: Set[A]) = lteq(a, b) && !lteq(b, a)

  def disjoint(a: Set[A], b: Set[A]) = {
    require(!a.isEmpty)
    require(!b.isEmpty)
    (a & b).isEmpty
  }
}

