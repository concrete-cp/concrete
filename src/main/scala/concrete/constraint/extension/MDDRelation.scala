package concrete.constraint.extension

import concrete.util.SetWithMax
import concrete.Variable
import scala.util.Random
import concrete.util.Timestamp

trait RelationGenerator {
  def apply(data: Seq[Array[Int]]): Relation
}

object MDDRelation extends RelationGenerator {
  def apply(data: Seq[Array[Int]]): MDDRelation = new MDDRelation(MDD(data))
}

final class MDDRelation(val mdd: MDD = MDD0, val timestamp: Timestamp = new Timestamp()) extends Relation {
  type Self2 = MDDRelation

  def findSupport(scope: Array[Variable], p: Int, i: Int, support: Array[Int]): Option[Array[Int]] = {
    assert(scope(p).dom.present(i))
    val s = mdd.findSupport(timestamp.next(), scope, p, i, support, 0)
    assert(s.forall(contains))
    assert(s.forall { sup =>
      (sup zip scope).forall(a => a._2.dom.present(a._1))
    })
    s
  }

  def edges: Int = {
    mdd.edges(timestamp.next())
  }

  def filterTrie(f: (Int, Int) => Boolean, modified: List[Int]): MDDRelation = {
    val m = mdd.filterTrie(timestamp.next(), f, modified, 0)

    assert(m.forall { sup =>
      sup.zipWithIndex.forall { case (i, p) => f(p, i) }
    }, modified)

    if (m eq mdd) {
      this
    } else {
      new MDDRelation(m, timestamp)
    }
  }

  def fillFound(f: (Int, Int) => Boolean, arity: Int): Traversable[Int] = {
    val l = new SetWithMax(arity)
    mdd.fillFound(timestamp.next(), f, 0, l)
    l
  }
  
  override def isEmpty = mdd.isEmpty

  def universal(scope: Array[Variable]) = mdd.universal(scope, timestamp.next())

  override def toString = s"$edges edges representing $lambda tuples"

  def copy: MDDRelation = new MDDRelation(mdd.copy(timestamp.next()))

  def lambda = mdd.lambda

  override def size = {
    val lambda = mdd.lambda
    require(lambda <= Int.MaxValue)
    lambda.toInt
  }

  def iterator = mdd.iterator.map(_.toArray)

  def -(t: Array[Int]) = throw new UnsupportedOperationException
  def +(t: Array[Int]) = new MDDRelation(mdd + t)
  def contains(t: Array[Int]): Boolean = mdd.contains(t)
}
