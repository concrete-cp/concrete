package cspfj.util
import cspfj.constraint.semantic.BDom
import scala.collection.immutable.BitSet
import scala.util.Random

case class HNode[A](
  val v: A,
  val rank: Int,
  val child: List[HNode[A]])

object Hasse {
  def empty[A](o: PartialOrdering[A], u: A, e: A) = new Hasse[A](o, HNode(u, 1, List(HNode(e, 0, Nil))))
}

class Hasse[A](val o: PartialOrdering[A], val root: HNode[A]) {

  def +(v: A): Hasse[A] = new Hasse[A](o, add(v, root))

  private def add(v: A, root: HNode[A]): HNode[A] = {
    //var subset = false

    if (o.lteq(v, root.v)) {
      HNode(root.v, root.rank + 1, root.child.map(add(v, _)))
    } else if (o.lteq(root.v, v)) {
      HNode(v, 1 + root.rank, List(root))
    } else root

  }

  override def toString: String = root.toString

}

class SetInclusion[A] extends PartialOrdering[Set[A]] {
  def ltEq(a: Set[A], b: Set[A]) =
    if (b == UniversalSet) true else a.subsetOf(b)
}

object UniversalSet extends BitSet

object Test {
  val rand = new Random

  def randSet() = {
    BitSet.empty ++ Stream.continually(rand.nextInt(10)).take(rand.nextInt(10))
  }

  def main(a: Array[String]) {
    var h: Hasse[(Int, BitSet)] = Hasse.empty(new PartialOrdering[(Int, BitSet)] {
      val si = new SetInclusion[Int]
      override def ltEq(a: (Int, BitSet), b: (Int, BitSet)) = si.ltEq(a._2, b._2)
    }, (0, UniversalSet), (0, BitSet.empty))
    var i = 0
    while (true) {
      h += (i, randSet())
      i += 1
    }
  }
}
