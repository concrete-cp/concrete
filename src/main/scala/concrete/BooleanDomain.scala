package concrete

import bitvectors.BitVector
import concrete.util.Interval

object BooleanDomain {

  case object UNKNOWNBoolean extends BooleanDomain {
    val bitVector = BitVector.filled(2)
    def singleValue = throw new IllegalStateException
    override def toString = "[f, t]"
    override def size = 2
    override def head = 0
    override def last = 1
    def next(i: Int) = if (i == 0) 1 else if (i < 0) 0 else throw new NoSuchElementException
    def prev(i: Int) = if (i == 1) 0 else if (i > 1) 1 else throw new NoSuchElementException
    def present(i: Int) = {
      Domain.checks += 1
      i == 0 || i == 1
    }
    def canBe(b: Boolean) = true
    def removeFrom(lb: Int) =
      if (lb <= 0) {
        EMPTY
      } else if (lb == 1) {
        FALSE
      } else {
        this
      }

    def removeAfter(lb: Int) =
      if (lb < 0) {
        EMPTY
      } else if (lb == 0) {
        FALSE
      } else {
        this
      }

    def removeTo(ub: Int) =
      if (ub >= 1) {
        EMPTY
      } else if (ub == 0) {
        TRUE
      } else {
        this
      }

    def removeUntil(ub: Int) =
      if (ub > 1) {
        EMPTY
      } else if (ub == 1) {
        TRUE
      } else {
        this
      }

    def remove(value: Int) = value match {
      case 0 => TRUE
      case 1 => FALSE
      case _ => throw new AssertionError("Out of bounds")
    }

    override def filter(f: Int => Boolean) =
      if (f(0)) {
        if (f(1)) {
          this
        } else {
          FALSE
        }
      } else {
        TRUE.filter(f)
      }

    val as01 = IntDomain.ofInterval(0, 1)
    val span = Interval(0, 1)

    def union(bd: BooleanDomain) = UNKNOWNBoolean

    def median = 1

    def isAssigned = false

    def disjoint(d: Domain) = !(d.present(0) || d.present(1))

    def shift(o: Int) = new IntervalDomain(o, o + 1)
  }

  case object TRUE extends BooleanDomain {
    val bitVector = BitVector.empty + 1
    override def toString = "[t]"
    def size = 1
    def singleValue = 1
    override def head = 1
    override def last = 1
    def next(i: Int) = throw new NoSuchElementException
    def prev(i: Int) = throw new NoSuchElementException
    def present(i: Int) = {
      Domain.checks += 1
      i == 1
    }
    def canBe(b: Boolean) = b
    def removeFrom(lb: Int) = if (lb <= 1) EMPTY else this
    def removeAfter(lb: Int) = if (lb < 1) EMPTY else this
    def removeTo(ub: Int) = if (ub >= 1) EMPTY else this
    def removeUntil(ub: Int) = if (ub > 1) EMPTY else this
    def remove(value: Int) = if (value == 1) EMPTY else this
    val as01 = Singleton(1)
    override def filter(f: Int => Boolean) = if (f(1)) this else EMPTY
    val span = Interval(1, 1)
    def union(bd: BooleanDomain) = bd match {
      case FALSE | UNKNOWNBoolean => UNKNOWNBoolean
      case _ => TRUE
    }
    def median = 1
    def isAssigned = true
    def disjoint(d: Domain) = !d.present(1)
    def shift(o: Int) = Singleton(o + 1)
  }

  case object FALSE extends BooleanDomain {
    val bitVector = BitVector.empty + 0
    override val toString = "[f]"
    def size = 1
    def singleValue = 0
    override def head = 0
    override def last = 0
    def next(i: Int) = throw new NoSuchElementException
    def prev(i: Int) = throw new NoSuchElementException
    def present(i: Int) = {
      Domain.checks += 1
      i == 0
    }
    def canBe(b: Boolean) = !b
    def removeFrom(lb: Int) = if (lb <= 0) EMPTY else this
    def removeAfter(lb: Int) = if (lb < 0) EMPTY else this
    def removeTo(ub: Int) = if (ub >= 0) EMPTY else this
    def removeUntil(ub: Int) = if (ub > 0) EMPTY else this
    def remove(value: Int) = if (value == 0) EMPTY else this
    val as01 = Singleton(0)
    override def filter(f: Int => Boolean) = if (f(0)) this else EMPTY
    val span = Interval(0, 0)
    def union(bd: BooleanDomain) = bd match {
      case TRUE | UNKNOWNBoolean => UNKNOWNBoolean
      case _ => FALSE
    }
    def median = 0
    def isAssigned = true
    def disjoint(d: Domain) = !d.present(0)
    def shift(o: Int) = Singleton(o)
  }

  case object EMPTY extends BooleanDomain {
    val bitVector = BitVector.empty
    override val toString = "[]"
    def size = 0
    def singleValue = throw new NoSuchElementException
    override def head = throw new NoSuchElementException
    override def last = throw new NoSuchElementException
    def next(i: Int) = throw new NoSuchElementException
    def prev(i: Int) = throw new NoSuchElementException
    def present(i: Int) = {
      Domain.checks += 1
      false
    }
    def canBe(b: Boolean) = false
    def removeFrom(lb: Int) = this
    def removeAfter(lb: Int) = this
    def removeTo(ub: Int) = this
    def removeUntil(ub: Int) = this
    def remove(v: Int) = this
    val as01 = EmptyIntDomain
    override def filter(f: Int => Boolean) = this
    def span = throw new NoSuchElementException
    def union(bd: BooleanDomain) = bd
    def median = throw new NoSuchElementException
    def isAssigned = throw new UnsupportedOperationException
    def disjoint(d: Domain): Boolean = true
    def shift(o: Int) = this
  }

  def apply(constant: Boolean): BooleanDomain = if (constant) TRUE else FALSE
  def apply(): BooleanDomain = UNKNOWNBoolean
}

sealed trait BooleanDomain extends Domain {

  def canBe(boolean: Boolean): Boolean

  def convex = true

  def as01: IntDomain

  def assign(value: Int) = {
    assert(0 <= value && value < 2)
    assert(present(value))
    if (value == 0) {
      BooleanDomain.FALSE
    } else {
      BooleanDomain.TRUE
    }

  }

  def bitVector: BitVector

  def toBitVector(offset: Int) = {
    require(offset == 0)
    bitVector
  }

  def &(lb: Int, ub: Int) = removeUntil(lb).removeAfter(ub)

  def &(d: Domain) = filter(i => d.present(i))

  def union(bd: BooleanDomain): BooleanDomain

  def |(d: Domain) = d match {
    case bd: BooleanDomain => this union bd
    case d => as01 | d
  }

  def filterBounds(f: Int => Boolean) = filter(f)

  def removeItv(from: Int, to: Int) = {
    val r0 = if (from <= 0 && 0 <= to) remove(0) else this
    if (from <= 1 && 1 <= to) r0.remove(1) else r0
  }

  def isEmpty = this eq BooleanDomain.EMPTY

  def foreach[S](f: Int => S): Unit = as01.foreach(f)

}

