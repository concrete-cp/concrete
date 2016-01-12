package concrete.constraint.extension

import concrete.util.SetWithMax
import concrete.Variable
import scala.util.Random
import concrete.util.Timestamp
import concrete.Domain
import com.typesafe.scalalogging.LazyLogging

object MDDLinkRelation extends RelationGenerator {
  def apply(data: Iterable[Seq[Int]]): MDDLinkRelation = new MDDLinkRelation(MDDLink(data.map(_.toList)))
}

final class MDDLinkRelation(val mdd: MDDLink, val timestamp: Timestamp = new Timestamp()) extends Relation with LazyLogging {
  type Self2 = MDDLinkRelation

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

  def edges: Int = mdd.edges(timestamp.next())

  def filterTrie(doms: Array[Domain], modified: List[Int]): MDDLinkRelation = {
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
      new MDDLinkRelation(m, timestamp)
    }
  }

  def fillFound(f: (Int, Int) => Boolean, arity: Int): Set[Int] = {
    val l = new SetWithMax(arity)
    mdd.fillFound(timestamp.next(), f, 0, l)
    l.toSet
  }

  override def isEmpty = mdd.isEmpty

  def universal(domains: IndexedSeq[Domain]) = mdd.universal(domains, timestamp.next())

  override def toString = s"link, $edges e for $lambda t"

  def copy: MDDLinkRelation = this

  lazy val lambda = mdd.lambda()

  override def size = {
    val lambda = mdd.lambda()
    require(lambda.isValidInt)
    lambda.toInt
  }

  def identify(): Int = mdd.identify()

  def iterator = mdd.iterator.map(_.toArray)

  def -(t: Seq[Int]) = throw new UnsupportedOperationException
  def +(t: Seq[Int]) = new MDDLinkRelation(mdd + t.toList)
  def contains(t: Array[Int]): Boolean = mdd.contains(t)
}
