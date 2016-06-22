package concrete.constraint.extension

import com.typesafe.scalalogging.LazyLogging
import concrete.Domain
import concrete.util.Timestamp
import cspom.extension.IdMap
import concrete.IntDomain

final class BDDRelation(val bdd: BDD, val timestamp: Timestamp = new Timestamp()) extends Relation with LazyLogging {
  type Self2 = BDDRelation

  def findSupport(domains: IndexedSeq[Domain], p: Int, i: Int): Option[Array[Int]] = {
    assert(domains(p).present(i))
    val support = new Array[Int](domains.length)
    val s = bdd.findSupport(timestamp.next(), domains, p, i, support, 0)
    assert(s.forall(contains))
    assert(s.forall { sup =>
      (sup zip domains).forall(a => a._2.present(a._1))
    })
    s
  }

  def edges: Int = bdd.edges(timestamp.next())

  def depth: Int = bdd.depth(new IdMap())

  def filterTrie(doms: Array[Domain], modified: List[Int]): BDDRelation = {
    val m = bdd.filterTrie(timestamp.next(), doms, modified, 0)

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
      new BDDRelation(m, timestamp)
    }
  }

  override def supported(doms: Array[Domain]): Array[IntDomain] =
    bdd.supported(timestamp.next(), doms)

  override def isEmpty = bdd.isEmpty

  def universal(domains: IndexedSeq[Domain]) = bdd.universal(domains, timestamp.next())

  override def toString = s"link, $edges e for $lambda t"

  def copy: BDDRelation = this

  lazy val lambda = bdd.lambda

  override def size = {
    require(lambda.isValidInt)
    lambda.toInt
  }

  def identify(): Int = bdd.identify()

  def iterator = bdd.iterator.map(_.toArray)

  def -(t: Seq[Int]) = throw new UnsupportedOperationException
  def +(t: Seq[Int]) = new BDDRelation(bdd + t.toList, timestamp)
  def contains(t: Array[Int]): Boolean = bdd.contains(t)
}
