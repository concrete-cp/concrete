package concrete

import concrete.util.Backtrackable
import concrete.util.BitVector
import concrete.util.IntSet
import concrete.util.Singleton
import concrete.util.EmptyIntSet

sealed trait Status {
  val bitVector = BitVector.newBitVector(2)
  def array: Array[Int]
  def size: Int
  def first: Int
  def last: Int
  def next(i: Int): Int
  def prev(i: Int): Int
  def present(i: Int): Boolean
  def canBe(b: Boolean): Boolean
  def removeFrom(lb: Int): Status
  def removeTo(ub: Int): Status
  def lowest(value: Int): Int
  def prevAbsent(value: Int): Int
  def lastAbsent: Int
  def indices: Array[Int]
  def intSet: IntSet
}

case object UNKNOWNBoolean extends Status {
  bitVector.fill(true)
  val array = Array(0, 1)
  override val toString = "[f, t]"
  def size = 2
  def first = 0
  def last = 1
  def next(i: Int) = if (i == 0) 1 else -1
  def prev(i: Int) = if (i == 1) 0 else -1
  def present(i: Int) = true
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
  def lowest(value: Int) =
    if (value > 1) { -1 }
    else if (value <= 0) { 0 }
    else { 1 }

  def lastAbsent = -1
  def prevAbsent(value: Int) = -1
  val indices = Array(0, 1)
  val intSet = IntSet.ofInterval(0, 1)
}

case object TRUE extends Status {
  bitVector.set(1)
  val array = Array(1)
  override val toString = "[t]"
  def size = 1
  def first = 1
  def last = 1
  def next(i: Int) = -1
  def prev(i: Int) = -1
  def present(i: Int) = i == 1
  def canBe(b: Boolean) = b
  def removeFrom(lb: Int) = if (lb == 0) EMPTY else this
  def removeTo(ub: Int) = if (ub == 1) EMPTY else this
  def lowest(value: Int) = if (value > 1) -1 else 1;
  def lastAbsent = 0
  def prevAbsent(value: Int) = if (value > 0) 0 else -1
  val indices = Array(1)
  val intSet = new Singleton(1)
}

case object FALSE extends Status {
  bitVector.set(0)
  val array = Array(0)
  override val toString = "[f]"
  def size = 1
  def first = 0
  def last = 0
  def next(i: Int) = -1
  def prev(i: Int) = -1
  def present(i: Int) = i == 0
  def canBe(b: Boolean) = !b
  def removeFrom(lb: Int) = if (lb <= 1) EMPTY else this
  def removeTo(ub: Int) = if (ub == 0) EMPTY else this
  def lowest(value: Int) = if (value > 0) -1 else 0;
  def lastAbsent = 1
  def prevAbsent(value: Int) = if (value > 0) 1 else -1
  val indices = Array(0)
  val intSet = new Singleton(0)
}

case object EMPTY extends Status {
  val array = Array[Int]()
  override val toString = "[]"
  def size = 0
  def first = -1
  def last = -1
  def next(i: Int) = -1
  def prev(i: Int) = -1
  def present(i: Int) = false
  def canBe(b: Boolean) = false
  def removeFrom(lb: Int) = this
  def removeTo(ub: Int) = this
  def lowest(value: Int) = -1
  def lastAbsent = 1
  def prevAbsent(value: Int) = if (value >= 1) 0 else -1
  val indices = Array[Int]()
  val intSet = EmptyIntSet
}

final class BooleanDomain(var _status: Status) extends Domain
  with Backtrackable[Status] {

  def this(constant: Boolean) = this(constant match {
    case true => TRUE
    case false => FALSE
  })

  def this() = this(UNKNOWNBoolean)

  def status = _status

  def save = status

  def restore(d: Status) { _status = d }

  def getAtLevel(l: Int) = getLevel(l).bitVector

  override def first = status.first

  override def size = status.size

  override def last = status.last

  def next(i: Int) = status.next(i)

  def prev(i: Int) = status.prev(i)

  /**
   * @param value
   *            the value we seek the index for
   * @return the index of the given value or -1 if it could not be found
   */
  def index(i: Int) = i

  /**
   * @param index
   *            index to test
   * @return true iff index is present
   */
  def present(i: Int) = {
    Domain.checks += 1
    status.present(i)
  }

  def setSingle(i: Int) {
    if (i == 0) {
      status = FALSE
    } else {
      status = TRUE
    }
  }

  def status_=(s: Status) {
    assert(status == UNKNOWNBoolean || s == EMPTY && status != EMPTY,
      "Assigning from " + status + " to " + s)
    _status = s;
    altering()
    if (s == EMPTY) throw EmptyDomain
  }

  def value(i: Int) = i

  def remove(i: Int) {
    assert(present(i));
    if (status.size == 1) {
      status = EMPTY
    } else if (i == 0) {
      status = TRUE
    } else {
      status = FALSE
    }
  }

  def filter(f: Int => Boolean) = {
    if (present(0) && !f(0)) {
      remove(0)
      true
    } else { false }
  } | {
    if (present(1) && !f(1)) {
      remove(1)
      true
    } else { false }
  }

  override def maxSize = 2

  //override val allValues = UNKNOWN.array

  override def toString = status.toString

  def getStatus = status

  def canBe(b: Boolean) = status.canBe(b)

  def closestLeq(value: Int) = throw new UnsupportedOperationException

  def removeFrom(lb: Int) = {
    val s = status.removeFrom(lb)
    if (s != status) {
      status = s
      true
    } else { false }
  }

  def removeTo(ub: Int) = {
    val s = status.removeTo(ub)
    if (s != status) {
      status = s;
      true
    } else { false }
  }

  def closestGeq(value: Int) = status.lowest(value)

  def closestGt(value: Int): Int = {
    if (value < 0) {
      if (status.canBe(false)) {
        0
      } else if (status.canBe(true)) {
        1
      } else {
        -1
      }
    } else if (value == 0 && status.canBe(true)) {
      1
    } else {
      -1
    }
  }

  def closestLt(value: Int): Int = {
    if (value > 1) {
      if (status.canBe(true)) {
        1
      } else if (status.canBe(false)) {
        0
      } else {
        -1
      }
    } else if (value == 1 && status.canBe(false)) {
      0
    } else {
      -1
    }
  }

  def prevAbsent(value: Int) = status.prevAbsent(value)

  def lastAbsent = status.lastAbsent

  def intersects(bv: BitVector) = bv.intersects(status.bitVector)
  def intersects(bv: BitVector, part: Int) = bv.intersects(status.bitVector, part)

  def isTrue = status eq TRUE
  def isFalse = status eq FALSE
  def isUnknown = status eq UNKNOWNBoolean
  override def isEmpty = status eq EMPTY
  def setTrue() { status = TRUE }
  def setFalse() { status = FALSE }
  def currentIndexes = status.indices
  def valueBV(offset: Int) = throw new UnsupportedOperationException
  def bound = true
  def intSet = status.intSet
  def undefined = false
}

