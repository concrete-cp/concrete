package concrete

import bitvectors.BitVector
import com.typesafe.scalalogging.LazyLogging
import concrete.util.{CacheOne, Interval}


final class BitVectorDomain(val offset: Int, val bitVector: BitVector, override val size: Int)
  extends IntDomain with LazyLogging {
  assert(size >= 2, "BitVectorSets must have at least two elements")
  assert(Math.addExact(offset, bitVector.lastSetBit) == offset + bitVector.lastSetBit)
  assert(bitVector.cardinality == size, s"$bitVector : ${bitVector.cardinality} != $size")

  lazy val span = Interval(head, last)
  override val last: Int = offset + bitVector.lastSetBit
  override val head: Int = offset + bitVector.nextSetBit(0)

  private val bvOffset = new CacheOne[Int, BitVector]()

  assert(iterator.size == size, s"${iterator.size} != $size for $offset, $bitVector, ${bitVector.lastSetBit} (${iterator.mkString(", ")})")

  def singleValue = throw new IllegalStateException(s"Tried to obtain single value of $this")

  override def prev(i: Int): Int = {
    val b = math.max(0, i - offset)
    val n = bitVector.prevSetBit(b)
    if (n < 0) throw new NoSuchElementException else offset + n
  }

  def isAssigned = false

  override def filterBounds(f: Int => Boolean): IntDomain = {
    val newbitVector = bitVector.filterBounds(i => f(i + offset))
    if (newbitVector eq bitVector) {
      this
    } else {
      IntDomain.ofBitVector(offset, newbitVector, newbitVector.cardinality)
    }
  }

  def excl(index: Int): IntDomain = {
    if (contains(index)) {
      IntDomain.ofBitVector(offset, bitVector - (index - offset), size - 1)
    } else {
      this
    }
  }

  /**
    * @param value to test
    * @return true iff value is present
    */
  def contains(value: Int): Boolean = {
    Domain.checks += 1
    val bit = value - offset
    bit >= 0 && bitVector.contains(bit)
  }

  def removeAfter(lb: Int): IntDomain = removeFrom(lb + 1)

  def removeFrom(lb: Int): IntDomain = {
    val b = lb - offset
    if (b <= bitVector.nextSetBit(0)) {
      EmptyIntDomain
    } else if (b > bitVector.lastSetBit) {
      this
    } else {
      val newbitVector = bitVector.clearFrom(b)
      IntDomain.ofBitVector(offset, newbitVector, newbitVector.cardinality)
    }
  }

  def &(lb: Int, ub: Int): IntDomain = {
    val blb = lb - offset
    val bub = ub - offset + 1

    val newBV = bitVector.clearFrom(bub).shift(-blb)

    val card = newBV.cardinality

    if (card == size) {
      this
    } else {
      assert(newBV.cardinality < size, s"$this (offset $offset) & ($lb, $ub) / $bitVector & ($blb, $bub) : no filtering but different instance $newBV")
      IntDomain.ofBitVector(lb, newBV, card)
    }
  }

  def removeTo(ub: Int): IntDomain = removeUntil(ub + 1)

  def removeUntil(ub: Int): IntDomain = {
    val b = ub - offset
    val first = bitVector.nextSetBit(0)
    if (b <= first) {
      this
    } else if (b > bitVector.lastSetBit) {
      EmptyIntDomain
    } else {
      val nbitVector = bitVector.shift(-b) //clearUntil(b)
      IntDomain.ofBitVector(ub, nbitVector, nbitVector.cardinality)
    }

  }

  override def toString: String =
    if (size <= IntDomain.DISPLAYED_VALUES) {
      view.mkString("{", ", ", "}");
    } else {
      view.take(IntDomain.DISPLAYED_VALUES - 1)
        .mkString("{", ", ", s", [${size - IntDomain.DISPLAYED_VALUES}...], $last}")
    }

  def shift(o: Int): IntDomain = if (o == 0) this else
    new BitVectorDomain(offset + o, bitVector, size)

  override def &(d: Domain): Domain = d match {
    case id: IntervalDomain => this & id.span
    case s: Singleton => if (contains(s.singleValue)) s else EmptyIntDomain
    case bd: BitVectorDomain => intersectBVD(bd)
    case EmptyIntDomain => EmptyIntDomain
    case b: BooleanDomain => b & this
    case _ => filter(d)
  }

  override def filter(f: Int => Boolean): IntDomain = {
    val newbitVector = if (offset == 0) bitVector.filter(f) else bitVector.filter(i => f(i + offset))
    if (newbitVector eq bitVector) {
      this
    } else {
      IntDomain.ofBitVector(offset, newbitVector, newbitVector.cardinality)
    }
  }

  def disjoint(d: Domain): Boolean = d match {
    case id: IntervalDomain => id.disjoint(this)
    case s: Singleton => s.disjoint(this)
    case bd: BitVectorDomain =>
      val newOffset = math.min(offset, bd.offset)
      toBitVector(newOffset).intersects(bd.toBitVector(newOffset)) < 0

    case EmptyIntDomain => true
    case b: BooleanDomain => b.disjoint(this)
    case _ => head > d.last || last < d.head || !exists(d)

  }

  def toBitVector(offset: Int): BitVector = {
    if (offset == this.offset) {
      bitVector
    } else {
      bvOffset(offset, {
        logger.trace(s"generating BV from offset ${this.offset} to $offset by ${Thread.currentThread().getStackTrace.toSeq}")
        bitVector.shift(this.offset - offset)
      })
    }
  }

  override def |(d: Domain): Domain = {
    d match {
      case s: Singleton => this | s.singleValue

      case id: IntervalDomain => this | id.span

      case bv: BitVectorDomain =>
        val newOffset = math.min(offset, bv.offset)
        val union = toBitVector(newOffset) | bv.toBitVector(newOffset)
        IntDomain.ofBitVector(newOffset, union, union.cardinality)

      case EmptyIntDomain | BooleanDomain.EMPTY => this

      case b: BooleanDomain => this | b.span
    }
  }

  def |(value: Int): IntDomain = {
    if (contains(value)) {
      this
    } else {
      val newOffset = math.min(value, offset)
      IntDomain.ofBitVector(newOffset, toBitVector(newOffset) + (value - newOffset), size + 1)
    }
  }

  def |(span: Interval): IntDomain = {
    val lb = span.lb
    val offset = math.min(lb, this.offset)
    val bv = toBitVector(offset)
    val union = bv.set(lb - offset, span.ub - offset + 1)
    IntDomain.ofBitVector(offset, union, union.cardinality)
  }

  def subsetOf(d: Domain): Boolean = {
    d match {
      case EmptyIntDomain | BooleanDomain.EMPTY | _: Singleton => assert(size > 1); false
      case d: BooleanDomain => head >= d.head && last <= d.last
      case id: IntervalDomain => (this & id.span).size == size
      case bd: BitVectorDomain => this.intersectBVD(bd).size == size
      case t: TreeSetDomain => last <= t.last && forall(t)
    }
  }

  private def intersectBVD(bd: BitVectorDomain): IntDomain = {
    val newOffset = math.min(offset, bd.offset)

    val thisBV = toBitVector(newOffset)
    val bdBV = bd.toBitVector(newOffset)

    val newBV = thisBV & bdBV

    val newCard = newBV.cardinality

    if (newCard == size) {
      this
    } else if (newCard == bd.size) {
      bd
    } else {
      IntDomain.ofBitVector(newOffset, newBV, newBV.cardinality)
    }
  }

  def convex = false

  override def isEmpty = false

  override def foreach[U](f: Int => U): Unit = {
    for (i <- bitVector) {
      f(i + offset)
    }
  }

  def median: Int = iterator.drop(size / 2).next

  def iterator: Iterator[Int] = new BVDIterator(head)

  def iteratorFrom(start: Int): Iterator[Int] = new BVDIterator(next(start - 1))

  override def next(i: Int): Int = {
    val b = math.max(0, i - offset + 1)
    val n = bitVector.nextSetBit(b)
    if (n < 0) throw new NoSuchElementException(s"${bitVector.getClass}$bitVector.next($i) with last = ${bitVector.lastSetBit}") else offset + n
  }



  final private class BVDIterator(private var current: Int) extends Iterator[Int] {
    // Assumes domain is not empty
    var hasNext = true

    def next(): Int = {
      val c = current
      if (c == BitVectorDomain.this.last) {
        hasNext = false
      } else {
        current = BitVectorDomain.this.next(current)
      }
      c
    }
  }

}