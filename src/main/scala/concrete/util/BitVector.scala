package concrete.util;
import BitVector._
final object BitVector {
  private val ADDRESS_BITS_PER_WORD = 6
  val WORD_SIZE = 1 << ADDRESS_BITS_PER_WORD
  val MASK = 0xFFFFFFFFFFFFFFFFL

  def intBv(lb: Int, ub: Int) = {
    BitVector.empty.set(lb, ub + 1)
  }

  def intBvH(lb: Int, ub: Int, hole: Int) = {
    intBv(lb, ub) - hole
  }

  val empty: BitVector = EmptyBitVector

  def filled(size: Int) = empty.set(0, size)

  def nbWords(nbBits: Int) = {
    if (nbBits % WORD_SIZE > 0) {
      word(nbBits) + 1
    } else {
      word(nbBits)
    }
  }

  def word(bit: Int) = bit >> ADDRESS_BITS_PER_WORD

}

trait BitVector extends Any {

  override def toString(): String = {
    val sb = new StringBuilder();
    sb.append('{');
    var i = nextSetBit(0);
    if (i != -1) {
      sb.append(i);
    }
    i = nextSetBit(i + 1)
    while (i != -1) {
      sb.append(", ").append(i);
      i = nextSetBit(i + 1)
    }
    sb.append('}').toString();

  }

  def set(position: Int, status: Boolean) = {
    if (status) {
      this + position
    } else {
      this - position
    }
  }

  def set(from: Int, until: Int): BitVector = {
    assert(from <= until)
    val startWordIndex = word(from)
    val maskFrom = MASK << from
    val lastWordIndex = word(until)
    val maskTo = MASK >>> -until

    val newWords = getWords.padTo(lastWordIndex + 1, 0L)
    val sw = newWords(startWordIndex)

    var changed = false
    if (startWordIndex == lastWordIndex) {
      newWords(startWordIndex) |= (maskFrom & maskTo)
    } else {
      newWords(startWordIndex) |= maskFrom

      val lw = newWords(lastWordIndex)
      newWords(lastWordIndex) |= maskTo

      changed |= (lw != newWords(lastWordIndex))

      for (i <- startWordIndex + 1 until lastWordIndex) {
        if (newWords(i) != MASK) {
          newWords(i) = MASK
          changed = true
        }
      }

    }
    changed |= (sw != newWords(startWordIndex))

    if (changed) {
      if (newWords.length == 1) {
        new SmallBitVector(newWords.head)
      } else {
        new LargeBitVector(newWords)
      }
    } else {
      this
    }
  }

  def getWords: Array[Long]

  def -(position: Int): BitVector

  def +(position: Int): BitVector = {
    val wordPos = word(position)
    val oldWord = getWord(wordPos)
    val newWord = oldWord | (1L << position)

    if (oldWord == newWord) {
      this
    } else {
      setWord(wordPos, newWord)
    }
  }

  def apply(position: Int): Boolean

  def nextSetBit(start: Int): Int

  def prevSetBit(start: Int): Int

  def lastSetBit: Int

  def clearFrom(from: Int): BitVector

  def clearUntil(until: Int): BitVector

  def intersects(bV: BitVector, position: Int): Boolean

  def intersects(bV: BitVector): Int;

  def nbWords: Int

  def ^(bv: BitVector): BitVector

  def &(bv: BitVector): BitVector

  def isEmpty(): Boolean

  def cardinality(): Int

  def getWord(i: Int): Long

  def subsetOf(bv: BitVector): Boolean

  def setWord(pos: Int, word: Long): BitVector

  def filter(f: Int => Boolean): BitVector
}

object EmptyBitVector extends BitVector {
  def &(bv: concrete.util.BitVector) = this
  def -(position: Int) = this
  def ^(bv: concrete.util.BitVector) = bv
  def apply(position: Int): Boolean = false
  def cardinality = 0
  def clearFrom(from: Int) = this
  def clearUntil(to: Int) = this
  def filter(f: Int => Boolean) = this
  def getWord(i: Int): Long = 0L
  def getWords: Array[Long] = Array()
  def intersects(bV: concrete.util.BitVector): Int = -1
  def intersects(bV: concrete.util.BitVector, position: Int): Boolean = false
  def isEmpty(): Boolean = true
  def lastSetBit: Int = -1
  def nbWords: Int = 0
  def nextSetBit(start: Int): Int = -1
  def prevSetBit(start: Int): Int = -1
  def setWord(pos: Int, word: Long) = {
    if (pos == 0) {
      new SmallBitVector(word)
    } else {
      val array = new Array[Long](pos + 1)
      array(pos) = word
      new LargeBitVector(array)
    }
  }
  def subsetOf(bv: concrete.util.BitVector): Boolean = true
}
