package concrete.constraint.extension

import java.util

import com.typesafe.scalalogging.LazyLogging
import concrete.Domain
import mdd.{MDD, SetWithMax}

object MDDRelation {
  def apply(data: Seq[Array[Int]]): MDDRelation =
    new MDDRelation(MDD.fromSeq(data))
}

final class MDDRelation(val mdd: MDD) extends Relation with LazyLogging {
  type Self2 = MDDRelation
  lazy val edges: Int = mdd.edges()
  lazy val lambda: BigInt = mdd.lambda()

  def findSupport(domains: Array[Domain], p: Int, i: Int): Option[Array[Int]] = {
    assert(domains(p).contains(i))
    val support = new Array[Int](domains.length)
    val s = mdd.findSupport(domains.asInstanceOf[Array[Set[Int]]], p, i, support)
    assert(s.forall(contains))
    assert(s.forall { sup =>
      (sup, domains).zipped.forall((s, d) => d.contains(s))
    })
    s
  }

  def contains(t: Array[Int]): Boolean = mdd.contains(t)

  def filterTrie(doms: Array[Domain], modified: List[Int]): MDDRelation = {
    val m = mdd.filterTrie(doms.asInstanceOf[Array[Set[Int]]], modified)

    //    assert(m.forall { sup =>
    //      sup.zipWithIndex.forall {
    //        case (i, p) =>
    //          val r = doms(p).present(i)
    //          if (!r) logger.warn(s"($p, $i) should have been filtered")
    //          r
    //      }
    //    }, modified + "\n" + m)

    if (m eq mdd) {
      this
    } else {
      new MDDRelation(m)
    }
  }

  def supported(doms: Array[Domain]): Array[util.HashSet[Int]] = {
    val newDomains = Array.fill(doms.length)(new util.HashSet[Int]())
    val l = new SetWithMax(doms.length)
    mdd.supported(doms.asInstanceOf[Array[Set[Int]]], newDomains, 0, l)
    newDomains
  }

  override def isEmpty: Boolean = mdd.isEmpty

  override def toString = s"$edges e for $lambda t"

  // def copy: MDDRelation = new MDDRelation(mdd.copy(timestamp.next()))

  def depth: Int = mdd.depth().get

  override def size: Int = {
    val lambda = mdd.lambda()
    require(lambda.isValidInt)
    lambda.toInt
  }

  def identify(): Int = mdd.identify()

  def iterator: Iterator[Array[Int]] = mdd.iterator.map(_.toArray)

  def -(t: Seq[Int]) = throw new UnsupportedOperationException

  def +(t: Seq[Int]) = throw new UnsupportedOperationException //new MDDRelation(mdd + t)
}
