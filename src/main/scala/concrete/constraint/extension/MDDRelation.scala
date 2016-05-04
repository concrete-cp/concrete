package concrete.constraint.extension

import com.typesafe.scalalogging.LazyLogging
import concrete.Domain
import concrete.util.SetWithMax
import concrete.util.Timestamp
import concrete.EmptyIntDomain
import concrete.IntDomain
import cspom.extension.IdMap

object MDDRelation extends RelationGenerator {
  def apply(data: Iterable[Seq[Int]]): MDDRelation = new MDDRelation(MDD(data))
}

final class MDDRelation(val mdd: MDD, val timestamp: Timestamp = new Timestamp()) extends Relation with LazyLogging {
  type Self2 = MDDRelation

  def findSupport(domains: IndexedSeq[Domain], p: Int, i: Int): Option[Array[Int]] = {
    assert(domains(p).present(i))
    val support = new Array[Int](domains.length)
    val s = mdd.findSupport(timestamp.next(), domains, p, i, support, 0)
    assert(s.forall(contains))
    assert(s.forall { sup =>
      (sup zip domains).forall(a => a._2.present(a._1))
    })
    s
  }

  lazy val edges: Int = mdd.edges(timestamp.next())

  def filterTrie(doms: Array[Domain], modified: List[Int]): MDDRelation = {
    val m = mdd.filterTrie(timestamp.next(), doms, modified, 0)

    assert(m.forall { sup =>
      sup.zipWithIndex.forall {
        case (i, p) =>
          val r = doms(p).present(i)
          if (!r) logger.warn(s"($p, $i) should have been filtered")
          r
      }
    }, modified + "\n" + m)

    if (m eq mdd) {
      this
    } else {
      new MDDRelation(m, timestamp)
    }
  }

  def supported(doms: Array[Domain]): Array[IntDomain] = {
    val newDomains = Array.fill[IntDomain](doms.length)(EmptyIntDomain)
    val l = new SetWithMax(doms.length)
    mdd.supported(timestamp.next(), doms, newDomains, 0, l)
    newDomains
  }

  override def isEmpty = mdd.isEmpty

  def universal(domains: IndexedSeq[Domain]) = mdd.universal(domains, timestamp.next())

  override def toString = s"$edges e for $lambda t"

  def copy: MDDRelation = new MDDRelation(mdd.copy(timestamp.next()))

  lazy val lambda = mdd.lambda
  
  def depth = mdd.depth(new IdMap())

  override def size = {
    val lambda = mdd.lambda
    require(lambda.isValidInt)
    lambda.toInt
  }

  def identify() = mdd.identify(0)

  def iterator = mdd.iterator.map(_.toArray)

  def -(t: Seq[Int]) = throw new UnsupportedOperationException
  def +(t: Seq[Int]) = new MDDRelation(mdd + t)
  def contains(t: Array[Int]): Boolean = mdd.contains(t)
}
