package cspfj.problem;

import java.util.Arrays
import cspfj.util.BitVector
import cspfj.UNSATException
import cspfj.util.Backtrackable

trait Status {
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
  def indexes: Array[Int]
}

object UNKNOWN extends Status {
  bitVector.fill(true)
  val array = Array(0, 1)
  override val toString = "[f, t]"
  val size = 2
  val first = 0
  val last = 1
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
    if (value > 1) -1
    else if (value <= 0) 0
    else 1

  val lastAbsent = -1
  def prevAbsent(value: Int) = -1
  val indexes = Array(0, 1)
}

object TRUE extends Status {
  bitVector.set(1)
  val array = Array(1)
  override val toString = "[t]"
  val size = 1
  val first = 1
  val last = 1
  def next(i: Int) = -1
  def prev(i: Int) = -1
  def present(i: Int) = i == 1
  def canBe(b: Boolean) = b
  def removeFrom(lb: Int) = if (lb == 0) EMPTY else this
  def removeTo(ub: Int) = if (ub == 1) EMPTY else this
  def lowest(value: Int) = if (value > 1) -1 else 1;
  val lastAbsent = 0
  def prevAbsent(value: Int) = if (value > 0) 0 else -1
  val indexes = Array(1)
}

object FALSE extends Status {
  bitVector.set(0)
  val array = Array(0)
  override val toString = "[f]"
  val size = 1
  val first = 0
  val last = 0
  def next(i: Int) = -1
  def prev(i: Int) = -1
  def present(i: Int) = i == 0
  def canBe(b: Boolean) = !b
  def removeFrom(lb: Int) = if (lb <= 1) EMPTY else this
  def removeTo(ub: Int) = if (ub == 0) EMPTY else this
  def lowest(value: Int) = if (value > 0) -1 else 0;
  val lastAbsent = 1
  def prevAbsent(value: Int) = if (value > 0) 1 else -1
  val indexes = Array(0)
}

object EMPTY extends Status {
  val array = Array[Int]()
  override val toString = "[]"
  val size = 0
  val first = -1
  val last = -1
  def next(i: Int) = -1
  def prev(i: Int) = -1
  def present(i: Int) = false
  def canBe(b: Boolean) = false
  def removeFrom(lb: Int) = this
  def removeTo(ub: Int) = this
  def lowest(value: Int) = -1
  val lastAbsent = 1
  def prevAbsent(value: Int) = if (value >= 1) 0 else -1
  val indexes = Array[Int]()
}

final class BooleanDomain(var _status: Status) extends Domain
  with Backtrackable[Status] {

  def this(constant: Boolean) = this(constant match {
    case true => TRUE
    case false => FALSE
  })

  def this() = this(UNKNOWN)

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
  def present(i: Int) = status.present(i)

  def setSingle(i: Int) {
    if (i == 0) {
      status = FALSE
    } else {
      status = TRUE
    }
  }

  def status_=(s: Status) {
    assert(status == UNKNOWN || s == EMPTY && status != EMPTY,
      "Assigning from " + status + " to " + s)
    _status = s;
    altering()
    if (s == EMPTY) throw Domain.empty
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

  def getBitVector = status.bitVector

  override val maxSize = 2

  //  def setLevel(level: Int) {
  //    assert(level > currentLevel)
  //    if (removed) {
  //      history ::= (currentLevel, this.status)
  //      removed = false
  //    }
  //    currentLevel = level;
  //  }
  //
  //  def restoreLevel(level: Int) {
  //    assert(level == 0 || level < currentLevel);
  //
  //    history = history.dropWhile(_._1 > level)
  //    _status = if (history == Nil) {
  //      UNKNOWN
  //    } else {
  //      history.head._2
  //    }
  //
  //    currentLevel = level;
  //  }
  //
  //  def getAtLevel(level: Int) = {
  //    if (level < currentLevel) {
  //      history.find(h => h._1 <= level) match {
  //        case Some(h) => h._2.bitVector
  //        case None => UNKNOWN.bitVector
  //      }
  //    } else status.bitVector;
  //  }

  override val allValues = UNKNOWN.array

  override def toString = status.toString

  def getStatus = status

  def canBe(b: Boolean) = status.canBe(b)

  def closestLeq(value: Int) = throw new UnsupportedOperationException

  def removeFrom(lb: Int) = {
    val s = size
    status = status.removeFrom(lb)
    size - s
  }

  def removeTo(ub: Int) = {
    val s = size
    status = status.removeTo(ub)
    size - s
  }

  def closestGeq(value: Int) = status.lowest(value)

  def prevAbsent(value: Int) = status.prevAbsent(value)

  def lastAbsent = status.lastAbsent

  def isTrue = status == TRUE
  def isFalse = status == FALSE
  def isUnknown = status == UNKNOWN
  override def isEmpty = status == EMPTY
  def setTrue() { status = TRUE }
  def setFalse() { status = FALSE }
  def currentIndexes = status.indexes
  def valueBV(offset: Int) = throw new UnsupportedOperationException
}

