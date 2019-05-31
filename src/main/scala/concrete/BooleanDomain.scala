package concrete

import bitvectors.BitVector
import concrete.util.Interval

object BooleanDomain {

  def apply(constant: Boolean): BooleanDomain = if (constant) TRUE else FALSE

  def apply(): BooleanDomain = UNKNOWNBoolean

  case object UNKNOWNBoolean extends BooleanDomain {
    val bitVector: BitVector = BitVector.filled(2)
    val as01: IntDomain = new IntervalDomain(0, 1)

    def singleValue = throw new IllegalStateException

    override def toString = "[f, t]"

    override def size = 2

    override def head = 0

    override def last = 1

    def next(i: Int): Int = if (i == 0) 1 else if (i < 0) 0 else throw new NoSuchElementException

    def prev(i: Int): Int = if (i == 1) 0 else if (i > 1) 1 else throw new NoSuchElementException

    def contains(i: Int): Boolean = {
      Domain.checks += 1
      i == 0 || i == 1
    }

    def canBe(b: Boolean) = true

    def removeFrom(lb: Int): BooleanDomain =
      if (lb <= 0) {
        EMPTY
      } else if (lb == 1) {
        FALSE
      } else {
        this
      }

    def removeAfter(lb: Int): BooleanDomain =
      if (lb < 0) {
        EMPTY
      } else if (lb == 0) {
        FALSE
      } else {
        this
      }

    def removeTo(ub: Int): BooleanDomain =
      if (ub >= 1) {
        EMPTY
      } else if (ub == 0) {
        TRUE
      } else {
        this
      }

    def removeUntil(ub: Int): BooleanDomain =
      if (ub > 1) {
        EMPTY
      } else if (ub == 1) {
        TRUE
      } else {
        this
      }

    def -(value: Int): BooleanDomain = value match {
      case 0 => TRUE
      case 1 => FALSE
      case _ => throw new AssertionError("Out of bounds")
    }

    override def filter(f: Int => Boolean): BooleanDomain = {
      if (f(0)) {
        if (f(1)) {
          this
        } else {
          FALSE
        }
      } else if (f(1)) {
        TRUE
      } else {
        EMPTY
      }
    }

    def union(bd: BooleanDomain): UNKNOWNBoolean.type = UNKNOWNBoolean

    def median = 1

    def isAssigned = false

    def disjoint(d: Domain): Boolean = !(d.contains(0) || d.contains(1))

    def shift(o: Int): Domain = as01.shift(o)

    def subsetOf(d: Domain): Boolean = d.contains(0) && d.contains(1)
  }

  case object TRUE extends BooleanDomain {
    val bitVector: BitVector = BitVector.empty + 1
    val as01 = Singleton(1)

    override def toString = "[t]"

    def length = 1

    def singleValue = 1

    override def head = 1

    override def last = 1

    def next(i: Int) = throw new NoSuchElementException

    def prev(i: Int) = throw new NoSuchElementException

    def contains(i: Int): Boolean = {
      Domain.checks += 1
      i == 1
    }

    def canBe(b: Boolean): Boolean = b

    def removeFrom(lb: Int): BooleanDomain = if (lb <= 1) EMPTY else this

    def removeAfter(lb: Int): BooleanDomain = if (lb < 1) EMPTY else this

    def removeTo(ub: Int): BooleanDomain = if (ub >= 1) EMPTY else this

    def removeUntil(ub: Int): BooleanDomain = if (ub > 1) EMPTY else this

    def -(value: Int): BooleanDomain = if (value == 1) EMPTY else this

    override def filter(f: Int => Boolean): BooleanDomain = if (f(1)) this else EMPTY

    def union(bd: BooleanDomain): BooleanDomain = bd match {
      case FALSE | UNKNOWNBoolean => UNKNOWNBoolean
      case _ => TRUE
    }

    def median = 1

    def isAssigned = true

    def disjoint(d: Domain): Boolean = !d.contains(1)

    def shift(o: Int) = Singleton(o + 1)

    def subsetOf(d: Domain): Boolean = d.contains(1)
  }

  case object FALSE extends BooleanDomain {
    override def toString = "[f]"
    val bitVector: BitVector = BitVector.filled(1)
    val as01 = Singleton(0)

    def length = 1

    def singleValue = 0

    override def head = 0

    override def last = 0

    def next(i: Int) = throw new NoSuchElementException

    def prev(i: Int) = throw new NoSuchElementException

    def contains(i: Int): Boolean = {
      Domain.checks += 1
      i == 0
    }

    def canBe(b: Boolean): Boolean = !b

    def removeFrom(lb: Int): BooleanDomain = if (lb <= 0) EMPTY else this

    def removeAfter(lb: Int): BooleanDomain = if (lb < 0) EMPTY else this

    def removeTo(ub: Int): BooleanDomain = if (ub >= 0) EMPTY else this

    def removeUntil(ub: Int): BooleanDomain = if (ub > 0) EMPTY else this

    def -(value: Int): BooleanDomain = if (value == 0) EMPTY else this

    override def filter(f: Int => Boolean): BooleanDomain = if (f(0)) this else EMPTY

    def union(bd: BooleanDomain): BooleanDomain = bd match {
      case TRUE | UNKNOWNBoolean => UNKNOWNBoolean
      case _ => FALSE
    }

    def median = 0

    def isAssigned = true

    def disjoint(d: Domain): Boolean = !d.contains(0)

    def shift(o: Int) = Singleton(o)

    def subsetOf(d: Domain): Boolean = d.contains(0)
  }

  case object EMPTY extends BooleanDomain {
    override def toString = "[]"
    val bitVector: BitVector = BitVector.empty
    val as01: IntDomain = EmptyIntDomain

    def length = 0

    def singleValue = throw new NoSuchElementException

    override def head = throw new NoSuchElementException

    override def last = throw new NoSuchElementException

    def next(i: Int) = throw new NoSuchElementException

    def prev(i: Int) = throw new NoSuchElementException

    def contains(i: Int): Boolean = {
      Domain.checks += 1
      false
    }

    def canBe(b: Boolean) = false

    def removeFrom(lb: Int): BooleanDomain = this

    def removeAfter(lb: Int): BooleanDomain = this

    def removeTo(ub: Int): BooleanDomain = this

    def removeUntil(ub: Int): BooleanDomain = this

    def -(v: Int): BooleanDomain = this

    override def filter(f: Int => Boolean): BooleanDomain = this

    def union(bd: BooleanDomain): BooleanDomain = bd

    def median = throw new NoSuchElementException

    def isAssigned = throw new UnsupportedOperationException

    def disjoint(d: Domain): Boolean = true

    def shift(o: Int): BooleanDomain = this

    def subsetOf(d: Domain) = true
  }

}

sealed trait BooleanDomain extends Domain {

  def canBe(boolean: Boolean): Boolean

  def convex = true

  def as01: IntDomain

  def assign(value: Int): BooleanDomain = {
    assert(0 <= value && value < 2)
    assert(contains(value))
    if (value == 0) {
      BooleanDomain.FALSE
    } else {
      BooleanDomain.TRUE
    }

  }

  def bitVector: BitVector

  def toBitVector(offset: Int): BitVector = {
    bitVector.shift(-offset)
  }

  def &(lb: Int, ub: Int): Domain = removeUntil(lb).removeAfter(ub)

  def &(d: Domain): Domain = filter(d)

  def union(bd: BooleanDomain): BooleanDomain

  def |(d: Domain): Domain = d match {
    case bd: BooleanDomain => this union bd
    case other => as01 | other
  }

  def filterBounds(f: Int => Boolean): Domain = filter(f)

  def removeItv(from: Int, to: Int): Domain = {
    val r0 = if (from <= 0 && 0 <= to) this - 0 else this
    if (from <= 1 && 1 <= to) r0 - 1 else r0
  }

  override def isEmpty: Boolean = this eq BooleanDomain.EMPTY

  override def foreach[S](f: Int => S): Unit = as01.foreach(f)

  def iterator: Iterator[Int] = as01.iterator

  def keysIteratorFrom(start: Int): Iterator[Int] = as01.keysIteratorFrom(start)

  def span: Interval = as01.span

  override def spanSlice(from: Option[Int], until: Option[Int]): Option[Interval] =
    as01.spanSlice(from, until)

}

