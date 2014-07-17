package concrete.util;
import BitVector._
final object BitVector {
  val ADDRESS_BITS_PER_WORD = 6
  val WORD_SIZE = 1 << ADDRESS_BITS_PER_WORD
  val MASK = 0xFFFFFFFFFFFFFFFFL

  def intBv(lb: Int, ub: Int) = {
    BitVector.empty.set(lb, ub)
  }

  def intBvH(lb: Int, ub: Int, hole: Int) = {
    intBv(lb, ub) - hole
  }

  val empty = new SmallBitVector(0)

  def filled(size: Int) = empty.set(0, size - 1)

  def nbWords(nbBits: Int) = {
    if (nbBits % WORD_SIZE > 0) {
      word(nbBits) + 1
    } else {
      word(nbBits)
    }
  }

  def word(bit: Int) = bit >> ADDRESS_BITS_PER_WORD

  def truncate(word: Long, size: Int): Long = word >>> -size

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

  def clearTo(to: Int): BitVector

  def set(from: Int, to: Int): BitVector

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
