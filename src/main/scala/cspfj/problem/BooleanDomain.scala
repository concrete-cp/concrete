package cspfj.problem;

import java.util.Arrays;

import cspfj.util.BitVector;

trait Status {
  def bitVector: BitVector
  def array: Array[Int]
  def size: Int
  def first: Int
  def last: Int
  def next(i: Int): Int
  def prev(i: Int): Int
  def present(i: Int): Boolean
  def canBe(b: Boolean): Boolean
  def removeFrom(lb: Int): Status
  def lowest(value: Int): Int
  def prevAbsent(value: Int): Int
  def lastAbsent: Int
}

object UNKNOWN extends Status {
  val bitVector = BitVector.newBitVector(2, true)
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
  def lowest(value: Int) =
    if (value > 1) -1
    else if (value <= 0) 0
    else 1

  val lastAbsent = -1
  def prevAbsent(value: Int) = -1
}

object TRUE extends Status {
  val bitVector = { val bv = BitVector.newBitVector(2, false); bv.set(1); bv }
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
  def lowest(value: Int) = if (value > 1) -1 else 1;
  val lastAbsent = 0
  def prevAbsent(value: Int) = -1
}

object FALSE extends Status {
  val bitVector = { val bv = BitVector.newBitVector(2, false); bv.set(0); bv }
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
  def lowest(value: Int) = if (value > 0) -1 else 0;
  val lastAbsent = 1
  def prevAbsent(value: Int) = -1
}

object EMPTY extends Status {
  val bitVector = BitVector.newBitVector(2, false)
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
  def lowest(value: Int) = -1
  val lastAbsent = 1
  def prevAbsent(value: Int) = if (value >= 1) 0 else -1
}

class BooleanDomain extends Domain {

  private var history: List[(Int, Status)] = Nil

  private var currentLevel = 0

  private var status: Status = UNKNOWN

  def this(constant: Boolean) = {
    this;
    status = constant match {
      case true => TRUE
      case false => FALSE
    }
  }

  override def firstIndex = status.first

  override def size = status.size

  override def lastIndex = status.last

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
      setStatus(FALSE);
    } else {
      setStatus(TRUE);
    }

  }

  def setStatus(status: Status) {
    assert(this.status == UNKNOWN || this.status == status)
    history ::= (currentLevel, this.status)
    this.status = status;

  }

  def value(i: Int) = i

  def remove(i: Int) {
    assert(present(i));
    if (status.size == 1) {
      setStatus(EMPTY);
    } else if (i == 0) {
      setStatus(TRUE);
    } else {
      setStatus(FALSE);
    }
  }

  def getBitVector = status.bitVector

  val maxSize = 2

  def setLevel(level: Int) {
    assert(level > currentLevel)
    currentLevel = level;
  }

  def restoreLevel(level: Int) {
    assert(level < currentLevel);

    if (history.head._1 >= level) {
      history = history.dropWhile(_._1 >= level)
      status = if (history == Nil) {
        UNKNOWN
      } else {
        history.head._2
      }
    }

    currentLevel = level;
  }

  def getAtLevel(level: Int) = {
    if (level < currentLevel) {
      history.find(h => h._1 <= level) match {
        case Some(h) => h._2
        case None => UNKNOWN.bitVector
      }
    }

    status.bitVector;
  }

  val allValues = UNKNOWN.array

  def currentValues = status.array

  override def toString = status.toString

  def getStatus = status

  def canBe(b: Boolean) = status.canBe(b)

  def closestLeq(value: Int) = throw new UnsupportedOperationException

  def removeFrom(lb: Int) = {
    val s = size
    status = status.removeFrom(lb)
    size - s
  }

  def removeTo(ub: Int) = throw new UnsupportedOperationException

  def closestGeq(value: Int) = status.lowest(value)

  def prevAbsent(value: Int) = status.prevAbsent(value)

  def lastAbsent = status.lastAbsent

  def isTrue = status == TRUE
  def isFalse = status == FALSE
  def isUnknown = status == UNKNOWN
  override def isEmpty = status == EMPTY
  def setTrue() { setStatus(TRUE) }
  def setFalse() { setStatus(FALSE) }
}

