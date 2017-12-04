package concrete

import bitvectors.{BitVector, BitVectorBuilder}
import com.typesafe.scalalogging.LazyLogging
import concrete.util.{CacheOne, Interval}

import scala.collection.immutable.TreeSet

final class TreeSetDomain(val set: TreeSet[Int])
  extends IntDomain with LazyLogging {

  lazy val span = Interval(head, last)
  override val head: Int = set.head

  override val last: Int = set.last
  private val offsetBV = new CacheOne[Int, BitVector]()

  def singleValue = throw new IllegalStateException

  override def next(i: Int): Int = {
    set.keysIteratorFrom(i + 1).next()
  }

  override def prev(i: Int): Int = {
    set.until(i).last
  }

  def isAssigned = false

  override def filter(f: Int => Boolean): IntDomain = {
    val newSet = set.filter(f)
    if (newSet.size == set.size) {
      this
    } else {
      IntDomain.ofTreeSet(newSet)
    }
  }

  override def filterBounds(f: Int => Boolean): IntDomain = {
    ???
  }

  def remove(index: Int): IntDomain = {
    assert(present(index))
    IntDomain.ofTreeSet(set - index)
  }

  def removeFrom(lb: Int): IntDomain = {
    if (lb > last) {
      this
    } else {
      IntDomain.ofTreeSet(set.until(lb))
    }
  }

  def removeAfter(lb: Int): IntDomain = {
    if (lb >= last) {
      this
    } else {
      IntDomain.ofTreeSet(set.to(lb))
    }
  }

  def removeUntil(ub: Int): IntDomain = {
    if (ub <= head) {
      this
    } else {
      IntDomain.ofTreeSet(set.from(ub))
    }
  }

  def removeTo(ub: Int): IntDomain = {
    if (ub < head) {
      this
    } else {
      IntDomain.ofTreeSet(set.from(ub + 1))
    }
  }

  def &(lb: Int, ub: Int): IntDomain = {
    if (lb > head || ub < last) {
      IntDomain.ofTreeSet(set.from(lb).to(ub))
    } else {
      this
    }
  }

  override def toString: String =
    if (size <= IntDomain.DISPLAYED_VALUES) {
      view.mkString("{", ", ", "}");
    } else {
      view.take(IntDomain.DISPLAYED_VALUES - 1)
        .mkString("{", ", ", s", [${size - IntDomain.DISPLAYED_VALUES}...], $last}")
    }

  //  var requestedOffset: Int = _
  //  var requestedBV: BitVector = _

  override def size: Int = set.size

  def subsetOf(d: IntDomain): Boolean =
    head >= d.head && last <= d.last && {
      d match {
        case _: IntervalDomain => true
        case _ => set.forall(d.present)
      }
    }

  def shift(o: Int): IntDomain = {
    new TreeSetDomain(set.map(_ + o))
  }


  assert(size >= 2, "Intervals must have at least two elements, use Singleton instead")

  def toBitVector(offset: Int): BitVector = {
    offsetBV(offset, {
      logger.info(s"generating BV for $this offset $offset")
      val bvb = new BitVectorBuilder(last - offset + 1)
      for (i <- set) {
        bvb += i - offset
      }
      bvb.result()
    })

  }

  def disjoint(d: Domain): Boolean = d match {
    case id: IntervalDomain => id.disjoint(this)
    case s: Singleton => s.disjoint(this)
    case EmptyIntDomain => true
    case b: BooleanDomain => b.disjoint(this)
    case _ => head > d.last || last < d.head || !set.exists(d.present)
  }

  override def |(d: Domain): Domain = {
    val newSet = set ++ d.view
    if (newSet.size > size) {
      IntDomain.ofTreeSet(newSet)
    } else {
      this
    }
  }

  def |(value: Int): IntDomain = {
    if (present(value)) {
      this
    } else {
      IntDomain.ofTreeSet(set + value)
    }
  }

  /**
    * @param value to test
    * @return true iff value is present
    */
  def present(value: Int): Boolean = {
    Domain.checks += 1
    set(value)
  }

  def |(span: Interval): IntDomain = {
    val newSet = set ++ span.allValues
    if (newSet.size > size) {
      IntDomain.ofTreeSet(newSet)
    } else {
      this
    }

  }

  def subsetOf(d: Domain): Boolean = {
    d match {
      case EmptyIntDomain | BooleanDomain.EMPTY | _: Singleton => assert(size > 1); false
      case d: BooleanDomain => head >= d.head && last <= d.last
      case id: IntervalDomain => (this & id.span).size == size
      case _ => last <= d.last && set.forall(d.present)
    }
  }

  override def &(d: Domain): Domain = d match {
    case id: IntervalDomain => this & id.span
    case s: Singleton => if (present(s.singleValue)) s else EmptyIntDomain
    case bd: BitVectorDomain => bd.filter(set)
    case EmptyIntDomain => EmptyIntDomain
    case b: BooleanDomain => b & this
    case ts: TreeSetDomain => IntDomain.ofTreeSet(set & ts.set)
  }

  //  def intersects(that: BitVector) = bitVector.intersects(that)
  //  def intersects(that: BitVector, part: Int) = bitVector.intersects(that, part)
  def convex = false

  override def isEmpty = false

  override def foreach[U](f: Int => U): Unit = set.foreach(f)

  def median: Int = iterator.drop(size / 2).next

  def iterator: Iterator[Int] = set.iterator

}
