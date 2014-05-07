package concrete.constraint.extension

import concrete.util.SetWithMax
import concrete.Variable

final class Timestamp {
  private var timestamp = 0
  def next(): Int = {
    timestamp += 1
    timestamp
  }
}

trait RelationGenerator {
  def apply(data: Seq[Array[Int]]): Relation
}

object MDDRelation extends RelationGenerator {
  def apply(data: Seq[Array[Int]]): MDDRelation = new MDDRelation(MDD(data))
}

final class MDDRelation(val mdd: MDD = MDD0, val timestamp: Timestamp = new Timestamp()) extends Relation {
  type Self2 = MDDRelation

  def find(f: (Int, Int) => Boolean): Option[Array[Int]] = {
    mdd.find(timestamp.next(), f, 0) map (_.toArray)
  }

  def findSupport(scope: Array[Variable], p: Int, i: Int, support: Array[Int]): Option[Array[Int]] = {
    mdd.findSupport(timestamp.next(), scope, p, i, support, 0)
  }

  def edges: Int = {
    mdd.edges(timestamp.next())
  }

  def filterTrie(f: (Int, Int) => Boolean, modified: List[Int]): MDDRelation = {
    new MDDRelation(mdd.filterTrie(timestamp.next(), f, modified, 0), timestamp)
  }

  def fillFound(f: (Int, Int) => Boolean, arity: Int): Traversable[Int] = {
    val l = new SetWithMax(arity)
    mdd.fillFound(timestamp.next(), f, 0, l)
    l
  }

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
