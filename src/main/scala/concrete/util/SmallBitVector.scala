package concrete.util;

import BitVector._

final class SmallBitVector(size: Int, val word: Long) extends BitVector(size) {
  require(size <= WORD_SIZE, s"Cannot handle vectors larger than $WORD_SIZE elements")

  def this(size: Int) = this(size, 0)

  def clear(position: Int) = {
    assert(position < size)
    val newWord = word & ~(1L << position)
    if (word == newWord) {
      this
    } else {
      new SmallBitVector(size, newWord)
    }
  }

  def set(position: Int) = {

    assert(position < size)
    val newWord = word | (1L << position)
    if (word == newWord) {
      this
    } else {
      new SmallBitVector(size, newWord)
    }
  }

  def apply(position: Int) = {
    position < size && (word & (1L << position)) != 0L;
  }

  def prevSetBit(start: Int) = {
    val prev = if (start >= WORD_SIZE) {
      word;
    } else {
      word & ~(MASK << start);
    }
    if (prev == 0) {
      -1;
    } else {

      WORD_SIZE - java.lang.Long.numberOfLeadingZeros(prev) - 1
    }
  }

  def prevClearBit(start: Int) = {
    val prev =
      if (start >= WORD_SIZE) {
        ~word;
      } else {
        ~word & ~(MASK << start);
      }
    if (prev == 0) { -1 } else { WORD_SIZE - java.lang.Long.numberOfLeadingZeros(prev) - 1 }
  }

  def nextSetBit(start: Int) = {
    if (start >= size) {
      -1;
    } else {
      val next = word & (MASK << start);
      if (next == 0) { -1 } else { java.lang.Long.numberOfTrailingZeros(next) }
    }
  }

  def clearFrom(from: Int) = {
    if (from >= size) { this }
    else {
      val newWord = word & ~(MASK << from)
      if (word == newWord) {
        this
      } else {
        new SmallBitVector(size, newWord)
      }

    }
  }

  def setFrom(from: Int) = {
    if (from >= size) {
      this

    } else {
      val newWord = (word | (MASK << from)) & (MASK >>> -size)
      if (word == newWord) {
        this
      } else {
        new SmallBitVector(size, newWord)
      }
    }

  }

  def clearTo(to: Int) = {
    if (to <= 0) {
      this
    } else {
      val newWord = word & ~(MASK >>> -to)
      if (word == newWord) {
        this
      } else {
        new SmallBitVector(size, newWord)
      }
    }
  }

  override def equals(o: Any) = o match {
    case bv: BitVector => bv.getWord(0) == word && bv.nextSetBit(WORD_SIZE) == -1
    case _ => false
  }

  override def hashCode = word.toInt

  def intersects(bv: BitVector, position: Int) = {
    (bv.getWord(0) & word) != 0;
  }

  def intersects(bv: BitVector): Int = {
    if (intersects(bv, 0)) 0 else -1;
  }

  def nbWords = 1

  def ^(bv: BitVector) = {
    bv.setFirstWord(bv.getWord(0) ^ this.word)
  }

  def &(bv: BitVector) = {
    val newWord = (bv.getWord(0) & this.word) & (MASK >>> -size)
    if (newWord == word) {
      this
    } else {
      new SmallBitVector(size, word)
    }
  }

  def unary_~() = {
    new SmallBitVector(size, ~this.word & (MASK >>> -size))
  }

  def isEmpty = word == 0L;

  def cardinality = java.lang.Long.bitCount(word);

  def subsetOf(bv: BitVector) = {
    (word & ~bv.getWord(0)) == 0L;
  }

  def getWord(i: Int) = {
    if (i > 0)
      0L;
    else
      word;
  }

  def setFirstWord(word: Long) = {
    new SmallBitVector(size, word)
  }
}
