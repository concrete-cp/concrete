package concrete.constraint.extension

import concrete.util.SetWithMax
import concrete.Variable
import scala.util.Random
import concrete.util.Timestamp
import concrete.Domain
import com.typesafe.scalalogging.LazyLogging

trait RelationGenerator {
  def apply(data: Seq[Seq[Int]]): Relation
}

object MDDRelation extends RelationGenerator {
  def apply(data: Seq[Seq[Int]]): MDDRelation = new MDDRelation(MDD(data))
}

final class MDDRelation(val mdd: MDD = MDD0, val timestamp: Timestamp = new Timestamp()) extends Relation with LazyLogging {
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

  def filterTrie(f: (Int, Int) => Boolean, modified: List[Int]): MDDRelation = {
    val m = mdd.filterTrie(timestamp.next(), f, modified, 0)

    assert(m.forall { sup =>
      sup.zipWithIndex.forall {
        case (i, p) =>
          val r = f(p, i)
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

  def fillFound(f: (Int, Int) => Boolean, arity: Int): Set[Int] = {
    val l = new SetWithMax(arity)
    mdd.fillFound(timestamp.next(), f, 0, l)
    l.toSet
  }

  override def isEmpty = mdd.isEmpty

  def universal(domains: IndexedSeq[Domain]) = mdd.universal(domains, timestamp.next())

  override def toString = s"$edges edges representing $lambda tuples"

  def copy: MDDRelation = new MDDRelation(mdd.copy(timestamp.next()))

  lazy val lambda = mdd.lambda

  override def size = {
    val lambda = mdd.lambda
    require(lambda.isValidInt)
    lambda.toInt
  }

  lazy val identify = mdd.identify(timestamp.next())

  def iterator = mdd.iterator.map(_.toArray)

  def -(t: Seq[Int]) = throw new UnsupportedOperationException
  def +(t: Seq[Int]) = new MDDRelation(mdd + t)
  def contains(t: Array[Int]): Boolean = mdd.contains(t)
}
