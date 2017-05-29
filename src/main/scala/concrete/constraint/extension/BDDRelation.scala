package concrete
package constraint
package extension

import com.typesafe.scalalogging.LazyLogging
import mdd.{BDD, IdMap, MiniSet}

final class BDDRelation(val bdd: BDD) extends Relation with LazyLogging {
  type Self2 = BDDRelation
  lazy val lambda = bdd.lambda()

  def findSupport(domains: Array[Domain], p: Int, i: Int): Option[Array[Int]] = {
    ???
  }

  def contains(t: Array[Int]): Boolean = bdd.contains(t)

  def vertices: Int = bdd.vertices()

  def depth: Int = bdd.depth(new IdMap())

  def filterTrie(doms: Array[Domain], modified: List[Int]): BDDRelation = {
    val m = bdd.filterTrie(doms.asInstanceOf[Array[MiniSet]], modified)

    assert(m.forall { sup =>
      sup.zipWithIndex.forall {
        case (i, p) =>
          val r = doms(p).present(i)
          if (!r) logger.warn(s"($p, $i) should have been filtered")
          r
      }
    }, modified + "\n" + m)

    if (m eq bdd) {
      this
    } else {
      new BDDRelation(m)
    }
  }

  override def supported(doms: Array[Domain]): Array[Domain] = {
    val (supp, offset) = bdd.supported(doms.asInstanceOf[Array[MiniSet]])
    supp.map(bv => IntDomain.ofBitVector(offset, bv, bv.cardinality))
  }

  override def isEmpty = bdd.isEmpty

  override def toString = s"link, $edges e for $lambda t"

  def edges: Int = bdd.edges()

  // def copy: BDDRelation = this

  override def size = {
    require(lambda.isValidInt)
    lambda.toInt
  }

  def iterator = bdd.iterator.map(_.toArray)

  def -(t: Seq[Int]) = throw new UnsupportedOperationException

  def +(t: Seq[Int]) = new BDDRelation(bdd + t.toList)
}
