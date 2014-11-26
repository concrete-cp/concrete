package concrete

import concrete.util.BitVector

case object UNKNOWNBoolean extends BooleanDomain {
  val toBitVector = BitVector.filled(2)
  override def toString = "[f, t]"
  override def length = 2
  override def head = 0
  override def last = 1
  def next(i: Int) = if (i <= 0) 1 else throw new NoSuchElementException
  def prev(i: Int) = if (i >= 1) 0 else throw new NoSuchElementException
  def present(i: Int) = { Domain.checks + 1; true }
  def canBe(b: Boolean) = true
  def removeFrom(lb: Int) = lb match {
    case 0 => EMPTY
    case 1 => FALSE
    case _ => this
  }
  def removeTo(ub: Int) = ub match {
    case 0 => TRUE
    case 1 => EMPTY
    case _ => this
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

  val as01 = IntDomain.ofInterval(0, 1)
}

case object TRUE extends BooleanDomain {
  val toBitVector = BitVector.empty + 1
  override def toString = "[t]"
  def length = 1
  override def head = 1
  override def last = 1
  def next(i: Int) = throw new NoSuchElementException
  def prev(i: Int) = throw new NoSuchElementException
  def present(i: Int) = { Domain.checks += 1; i == 1 }
  def canBe(b: Boolean) = b
  def removeFrom(lb: Int) = if (lb == 0) EMPTY else this
  def removeTo(ub: Int) = if (ub == 1) EMPTY else this
  def nextOrEq(value: Int) = if (value > 1) throw new NoSuchElementException else 1;
  def prevOrEq(value: Int) = if (value < 1) throw new NoSuchElementException else 1;
  def remove(value: Int) = if (value == 1) EMPTY else this
  val as01 = new Singleton(1)
}

case object FALSE extends BooleanDomain {
  val toBitVector = BitVector.empty + 0
  override val toString = "[f]"
  def length = 1
  override def head = 0
  override def last = 0
  def next(i: Int) = throw new NoSuchElementException
  def prev(i: Int) = throw new NoSuchElementException
  def present(i: Int) = { Domain.checks += 1; i == 0 }
  def canBe(b: Boolean) = !b
  def removeFrom(lb: Int) = if (lb <= 1) EMPTY else this
  def removeTo(ub: Int) = if (ub == 0) EMPTY else this
  def nextOrEq(value: Int) = if (value > 0) throw new NoSuchElementException else 0;
  def prevOrEq(value: Int) = if (value < 0) throw new NoSuchElementException else 0;
  def remove(value: Int) = if (value == 0) EMPTY else this
  val as01 = new Singleton(0)
}

case object EMPTY extends BooleanDomain {
  val toBitVector = BitVector.empty
  override val toString = "[]"
  def length = 0
  override def head = throw new NoSuchElementException
  override def last = throw new NoSuchElementException
  def next(i: Int) = throw new NoSuchElementException
  def prev(i: Int) = throw new NoSuchElementException
  def present(i: Int) = false
  def canBe(b: Boolean) = false
  def removeFrom(lb: Int) = this
  def removeTo(ub: Int) = this
  def nextOrEq(value: Int) = throw new NoSuchElementException
  def prevOrEq(value: Int) = throw new NoSuchElementException
  def remove(v: Int) = this
  val as01 = EmptyIntDomain
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
    assert(present(value))
    value match {
      case 0 => FALSE
      case 1 => TRUE
      case _ => throw new IllegalArgumentException
    }
  }

  def iterator = as01.iterator

  def apply(i: Int) = as01.apply(i)
}

