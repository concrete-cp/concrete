package concrete.util;

object BitVector {
  val ADDRESS_BITS_PER_WORD = 6
  val WORD_SIZE = 1 << ADDRESS_BITS_PER_WORD
  val MASK = 0xFFFFFFFFFFFFFFFFL

  def intBv(lb: Int, ub: Int) = {
    val bv = BitVector(ub + 1);
    bv.setFrom(lb);
    bv;
  }

  def intBvH(lb: Int, ub: Int, hole: Int) = {
    val bv = intBv(lb, ub);
    bv.clear(hole);
    bv;
  }

  def apply(size: Int) = {
    if (size > WORD_SIZE) {
      new LargeBitVector(size);
    } else {
      new SmallBitVector(size);
    }
  }

  def nbWords(nbBits: Int) = {
    if (nbBits % WORD_SIZE > 0) {
      word(nbBits) + 1
    } else {
      word(nbBits)
    }
  }

  def word(bit: Int) = bit >> ADDRESS_BITS_PER_WORD

}

abstract class BitVector(val size: Int) {

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

  def clear(position: Int): BitVector

  def set(position: Int): BitVector

  def apply(position: Int): Boolean

  def nextSetBit(start: Int): Int

  def prevSetBit(start: Int): Int

  def lastSetBit = prevSetBit(size)

  def prevClearBit(start: Int): Int

  def lastClearBit: Int = prevClearBit(size)

  /**
   * Removes all values from given bound (included).
   *
   * @param from
   * @return How many values were actually removed
   */
  def clearFrom(from: Int): BitVector

  def setFrom(from: Int): BitVector

  /**
   * Removes all values up to given bound (excluded).
   *
   * @param ub
   * @return How many values were actually removed
   */
  def clearTo(ub: Int): BitVector

  def intersects(bV: BitVector, position: Int): Boolean

  def intersects(bV: BitVector): Int;

  def nbWords(): Int

  def ^(bv: BitVector): BitVector

  def &(bv: BitVector): BitVector

  def unary_~(): BitVector

  def isEmpty(): Boolean

  def cardinality(): Int

  def getWord(i: Int): Long

  def subsetOf(bv: BitVector): Boolean

  def setFirstWord(word: Long): BitVector

}
