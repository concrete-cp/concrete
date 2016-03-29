package concrete.constraint.extension

import java.lang.ref.WeakReference
import scala.annotation.elidable
import scala.annotation.elidable.ASSERTION
import com.typesafe.scalalogging.LazyLogging
import concrete.Domain
import concrete.util.SetWithMax
import concrete.util.Timestamp
import cspom.extension.IdMap
import concrete.IntDomain
import concrete.EmptyIntDomain

object BDD {

  type Cache = collection.mutable.Map[BDDRNode, WeakReference[BDDRNode]]

}

trait BDD extends Iterable[Seq[Int]] {

  final type Cache = BDD.Cache

  def id: Int
  def +(e: List[Int])(implicit cache: Cache): BDD
  def lambda: BigInt = lambda(new IdMap())
  protected[extension] def lambda(map: IdMap[BDD, BigInt]): BigInt
  def contains(e: Seq[Int]): Boolean
  def edges(ts: Int): Int
  def filterTrie(ts: Int, doms: Array[Domain], modified: List[Int], depth: Int, cache: Cache): BDD

  def supported(ts: Int, domains: Array[Domain]): Array[IntDomain] = {
    val arity = domains.length
    val newDomains = Array.fill[IntDomain](arity)(EmptyIntDomain)

    def updNewDomains(depth: Int, i: Int) = {
      ReduceableExt.fills += 1
      newDomains(depth) |= i
      newDomains(depth).length == domains(depth).length
    }

    fillFound(ts, updNewDomains, 0, new SetWithMax(arity))

    newDomains
  }

  def fillFound(ts: Int, f: (Int, Int) => Boolean, depth: Int, l: SetWithMax): Unit

  def findSupport(ts: Int, scope: IndexedSeq[Domain], p: Int, i: Int, support: Array[Int], depth: Int): Option[Array[Int]] = {
    ???
  }
  def universal(scope: IndexedSeq[Domain], timestamp: Int): Boolean = ???

  def nodes(map: IdMap[BDD, Unit]): IdMap[BDD, Unit]
  def identify(): Int
}

final class BDDRelation(val bdd: BDD, val timestamp: Timestamp = new Timestamp(), implicit val cache: BDD.Cache) extends Relation with LazyLogging {
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

  def filterTrie(doms: Array[Domain], modified: List[Int]): BDDRelation = {
    val m = bdd.filterTrie(timestamp.next(), doms, modified, 0, cache)

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
      new BDDRelation(m, timestamp, cache)
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
  def +(t: Seq[Int]) = new BDDRelation(bdd + t.toList, timestamp, cache)
  def contains(t: Array[Int]): Boolean = bdd.contains(t)
}
