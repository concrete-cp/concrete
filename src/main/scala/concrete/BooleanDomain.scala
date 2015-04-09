package concrete

import concrete.util.BitVector
import concrete.util.Interval

case object UNKNOWNBoolean extends BooleanDomain {
  val bitVector = BitVector.filled(2)
  def singleValue = throw new IllegalStateException
  override def toString = "[f, t]"
  override def length = 2
  override def head = 0
  override def last = 1
  def next(i: Int) = if (i == 0) 1 else if (i < 0) 0 else throw new NoSuchElementException
  def prev(i: Int) = if (i == 1) 0 else if (i > 1) 1 else throw new NoSuchElementException
  def present(i: Int) = { Domain.checks + 1; true }
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

  def nextOrEq(value: Int) =
    if (value <= 0) 0
    else if (value <= 1) 1
    else throw new NoSuchElementException

  def prevOrEq(value: Int) = {
    if (value >= 1) { 1 }
    else if (value >= 0) { 0 }
    else { throw new NoSuchElementException }
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
}

case object TRUE extends BooleanDomain {
  val bitVector = BitVector.empty + 1
  override def toString = "[t]"
  def length = 1
  def singleValue = 1
  override def head = 1
  override def last = 1
  def next(i: Int) = throw new NoSuchElementException
  def prev(i: Int) = throw new NoSuchElementException
  def present(i: Int) = { Domain.checks += 1; i == 1 }
  def canBe(b: Boolean) = b
  def removeFrom(lb: Int) = if (lb <= 1) EMPTY else this
  def removeAfter(lb: Int) = if (lb < 1) EMPTY else this
  def removeTo(ub: Int) = if (ub >= 1) EMPTY else this
  def removeUntil(ub: Int) = if (ub > 1) EMPTY else this
  def nextOrEq(value: Int) = if (value > 1) throw new NoSuchElementException else 1;
  def prevOrEq(value: Int) = if (value < 1) throw new NoSuchElementException else 1;
  def remove(value: Int) = if (value == 1) EMPTY else this
  val as01 = new Singleton(1)
  override def filter(f: Int => Boolean) = if (f(1)) this else EMPTY
  val span = Interval(1, 1)
}

case object FALSE extends BooleanDomain {
  val bitVector = BitVector.empty + 0
  override val toString = "[f]"
  def length = 1
  def singleValue = 0
  override def head = 0
  override def last = 0
  def next(i: Int) = throw new NoSuchElementException
  def prev(i: Int) = throw new NoSuchElementException
  def present(i: Int) = { Domain.checks += 1; i == 0 }
  def canBe(b: Boolean) = !b
  def removeFrom(lb: Int) = if (lb <= 0) EMPTY else this
  def removeAfter(lb: Int) = if (lb < 0) EMPTY else this
  def removeTo(ub: Int) = if (ub >= 0) EMPTY else this
  def removeUntil(ub: Int) = if (ub > 0) EMPTY else this
  def nextOrEq(value: Int) = if (value > 0) throw new NoSuchElementException else 0;
  def prevOrEq(value: Int) = if (value < 0) throw new NoSuchElementException else 0;
  def remove(value: Int) = if (value == 0) EMPTY else this
  val as01 = new Singleton(0)
  override def filter(f: Int => Boolean) = if (f(0)) this else EMPTY
  val span = Interval(0, 0)
}

case object EMPTY extends BooleanDomain {
  val bitVector = BitVector.empty
  override val toString = "[]"
  def length = 0
  def singleValue = throw new NoSuchElementException
  override def head = throw new NoSuchElementException
  override def last = throw new NoSuchElementException
  def next(i: Int) = throw new NoSuchElementException
  def prev(i: Int) = throw new NoSuchElementException
  def present(i: Int) = false
  def canBe(b: Boolean) = false
  def removeFrom(lb: Int) = this
  def removeAfter(lb: Int) = this
  def removeTo(ub: Int) = this
  def removeUntil(ub: Int) = this
  def nextOrEq(value: Int) = throw new NoSuchElementException
  def prevOrEq(value: Int) = throw new NoSuchElementException
  def remove(v: Int) = this
  val as01 = EmptyIntDomain
  override def filter(f: Int => Boolean) = this
  def span = throw new NoSuchElementException
}

object BooleanDomain {
  def apply(constant: Boolean): BooleanDomain = if (constant) TRUE else FALSE
  def apply(): BooleanDomain = UNKNOWNBoolean
}

sealed trait BooleanDomain extends Domain {

  def canBe(boolean: Boolean): Boolean

  def bound = true

  def as01: IntDomain

  def assign(value: Int) = {
    if (present(value)) {
      value match {
        case 0 => FALSE
        case 1 => TRUE
        case _ => throw new IllegalArgumentException
      }
    } else {
      EMPTY
    }
  }

  def iterator = as01.iterator

  def apply(i: Int) = as01.apply(i)

  def bitVector: BitVector

  def bitVector(offset: Int) = {
    require(offset == 0)
    bitVector
  }

}

