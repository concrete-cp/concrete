package concrete

import bitvectors.BitVector
import com.typesafe.scalalogging.LazyLogging
import concrete.util.{CacheOne, Interval}


final class BitVectorDomain(val offset: Int, val bitVector: BitVector, override val size: Int)
  extends IntDomain with LazyLogging {
  assert(size >= 2, "BitVectorSets must have at least two elements")
  assert(Math.addExact(offset, bitVector.lastSetBit) == offset + bitVector.lastSetBit)

  assert(bitVector.cardinality == size, bitVector + " : " + bitVector.cardinality + " != " + size)

  assert(view.size == size)
  //  def this(size: Int) = {
  //    this(0, BitVector.filled(size), size)
  //  }

  lazy val span = Interval(head, last)
  override def head: Int = {

    assert(view.size == size)
    offset + bitVector.nextSetBit(0)
  }

  assert(head >= offset)

  override val last: Int = offset + bitVector.lastSetBit

  assert(last >= offset)
  private val bvOffset = new CacheOne[Int, BitVector]()

  def singleValue = throw new IllegalStateException(s"Tried to obtain single value of $this")

  override def prev(i: Int): Int = {
    val b = math.max(0, i - offset)
    val n = bitVector.prevSetBit(b)
    if (n < 0) throw new NoSuchElementException else offset + n
  }

  def isAssigned = false

  override def filterBounds(f: Int => Boolean): IntDomain = {
    val newbitVector = bitVector.filterBounds(i => f(i + offset))
    if (newbitVector == bitVector) {
      this
    } else {
      IntDomain.ofBitVector(offset, newbitVector, newbitVector.cardinality)
    }
  }

  def remove(index: Int): IntDomain = {
    assert(present(index))
    //if (present(index)) {
    IntDomain.ofBitVector(offset, bitVector - (index - offset), size - 1)
    //} else { this }
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

//  override def subsetOf(d: Domain): Boolean = d match {
//    case d: BitVectorDomain => {
//      val no = math.min(offset, d.offset)
//      bitVector.shift(offset - no).subsetOf(d.bitVector.shift(d.offset - no))
//    }
//    case d: IntervalDomain => head >= d.head && last <= d.last
//  }

  def shift(o: Int): IntDomain = if (o == 0) this else
    new BitVectorDomain(offset + o, bitVector, size)

  //  var requestedOffset: Int = _
  //  var requestedBV: BitVector = _

  override def &(d: Domain): Domain = d match {
    case id: IntervalDomain => this & id.span
    case s: Singleton => if (present(s.singleValue)) s else EmptyIntDomain
    case bd: BitVectorDomain => intersectBVD(bd)
    case EmptyIntDomain => EmptyIntDomain
    case b: BooleanDomain => b & this
    case _ => filter(d.present)
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

  /**
    * @param value to test
    * @return true iff value is present
    */
  def present(value: Int): Boolean = {
    Domain.checks += 1
    val bit = value - offset
    bit >= 0 && bitVector(bit)
  }

  override def filter(f: Int => Boolean): IntDomain = {
    val newbitVector = if (offset == 0) bitVector.filter(f) else bitVector.filter(i => f(i + offset))
    if (newbitVector == bitVector) {
      this
    } else {
      IntDomain.ofBitVector(offset, newbitVector, newbitVector.cardinality)
    }
  }

  def toBitVector(offset: Int): BitVector = {
    if (offset == this.offset) {
      bitVector
    } else {
      //      if (requestedBV != null && offset == requestedOffset) {
      //    }
      //      requestedBV
      //    } else {

      bvOffset(offset, {
        logger.trace(s"generating BV from offset ${this.offset} to $offset by ${Thread.currentThread().getStackTrace.toSeq}")
        // requestedOffset = offset
        bitVector.shift(this.offset - offset)
        //        BitVector(this.view.map(_ - offset))
        //        //        for (v <- this) {
        //        //          requestedBV += v - offset
        //        //        }
        //        requestedBV
      })
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
    case _ => head > d.last || last < d.head || !exists(d.present)

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
    if (present(value)) {
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
      case EmptyIntDomain | BooleanDomain.EMPTY | _:Singleton => assert(size > 1); false
      case d: BooleanDomain => head >= d.head && last <= d.last
      case id: IntervalDomain => (this & id.span).size == size
      case bd: BitVectorDomain => this.intersectBVD(bd).size == size
      case t: TreeSetDomain => last <= t.last && iterator.forall(t.present)
    }
  }

  //  def intersects(that: BitVector) = bitVector.intersects(that)
  //  def intersects(that: BitVector, part: Int) = bitVector.intersects(that, part)
  def convex = false

  override def isEmpty = false

  override def foreach[U](f: Int => U): Unit = {
    for (i <- bitVector) {
      f(i + offset)
    }
  }

  def median: Int = iterator.drop(size / 2).next

  def iterator: Iterator[Int] = {
    new Iterator[Int] {
    private var current = BitVectorDomain.this.head
    // Assumes domain is not empty
    var hasNext = true

    def next(): Int = {
      val c = current
      if (current == last) {
        hasNext = false
      } else {
        current = BitVectorDomain.this.next(current)
      }
      c
      //} else Iterator.empty.next()

    }
  }}

  override def next(i: Int): Int = {
    val b = math.max(0, i - offset + 1)
    val n = bitVector.nextSetBit(b)
    if (n < 0) throw new NoSuchElementException(s"${bitVector.getClass}$bitVector.next($i) with last = ${bitVector.lastSetBit}") else offset + n
  }

}
