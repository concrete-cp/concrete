package concrete.util;

import java.util.Arrays;
import BitVector._

class LargeBitVector(val words: Array[Long]) extends AnyVal with BitVector {

  def -(position: Int) = {
    val wordPos: Int = word(position)
    val oldWord = words(wordPos)
    val newWord = oldWord & ~(1L << position)
    if (oldWord == newWord) {
      this
    } else {
      setWord(wordPos, newWord)
    }
  }

  def apply(position: Int) = {
    val wordPos = word(position)
    wordPos < nbWords && (words(wordPos) & (1L << position)) != 0L
  }

  def nextSetBit(start: Int): Int = {
    var position = word(start);

    if (position >= nbWords) {
      -1
    } else {
      var word = words(position) & (MASK << start);

      while (word == 0) {
        position += 1
        if (position == nbWords) {
          return -1;
        }
        word = words(position);
      }
      (position * WORD_SIZE) + java.lang.Long.numberOfTrailingZeros(word);
    }
  }

  def prevSetBit(start: Int): Int = {

    val wordsInUse = words.length;
    val startWord = BitVector.word(start)
    var position: Int = math.min(wordsInUse - 1, startWord);

    var word = words(position);
    if (position == startWord) {
      word &= ~(MASK << start)
    }

    while (word == 0) {
      position -= 1
      if (position < 0) {
        return -1;
      }
      word = words(position)
    }
    return (1 + position) * WORD_SIZE - java.lang.Long.numberOfLeadingZeros(word) - 1;

  }

  def prevClearBit(start: Int): Int = {
    val wordsInUse = words.length;
    val startWord = BitVector.word(start)
    var position = math.min(wordsInUse - 1, startWord);

    var word = ~words(position);
    if (position == startWord) {
      word &= ~(MASK << start);
    }

    while (word == 0) {
      position -= 1
      if (position < 0) {
        return -1;
      }
      word = ~words(position)
    }
    (1 + position) * WORD_SIZE - java.lang.Long.numberOfLeadingZeros(word) - 1;
  }

  def ^(bv: BitVector) = {
    val n = math.max(nbWords, bv.nbWords)

    val words = Array.ofDim[Long](n)
    var i = n - 1
    while (i >= 0) {
      words(i) = getWord(i) ^ bv.getWord(i);
      i -= 1
    }
    new LargeBitVector(words)
  }

  def &(bv: BitVector) = {
    val n = math.min(nbWords, bv.nbWords)
    val newWords = Array.ofDim[Long](n)
    var i = n - 1
    while (i >= 0) {
      newWords(i) = bv.getWord(i) & words(i);
      i -= 1
    }
    new LargeBitVector(newWords)
  }

  def clearFrom(from: Int) = {
    if (from <= 0) {
      BitVector.empty
    } else {
      val startWordIndex = word(from)

      if (startWordIndex >= nbWords) {
        this
      } else if (startWordIndex == 0) {
        new SmallBitVector(words(0) & ~(MASK << from))
      } else {

        val newWords = words.take(startWordIndex + 1)

        val w = newWords(startWordIndex);

        newWords(startWordIndex) &= ~(MASK << from);

        if (newWords.length < nbWords || w != newWords(startWordIndex)) {
          new LargeBitVector(newWords)
        } else {
          this
        }
      }
    }
  }

  def clearUntil(until: Int) = {
    if (until < 0) {
      this
    } else {
      val endWordIndex = word(until);
      val newWords = words.clone
      val w = newWords(endWordIndex)
      // Handle first word
      newWords(endWordIndex) &= (MASK << until);
      var removed = w != newWords(endWordIndex);

      // Handle intermediate words, if any
      for (i <- 0 until endWordIndex) {
        if (newWords(i) != 0) {
          newWords(i) = 0;
          removed = true;
        }
      }

      if (removed) {
        new LargeBitVector(newWords)
      } else {
        this
      }
    }
  }

  //  override def equals(o: Any): Boolean = o match {
  //    case bv: BitVector =>
  //      for (i <- 0 until nbWords) {
  //        if (getWord(i) != bv.getWord(i)) {
  //          return false;
  //        }
  //      }
  //      true;
  //    case _ => false
  //  }
  //
  //  override def hashCode(): Int = {
  //    var result = 721L;
  //    for (w <- words) {
  //      result = 31 * result + w;
  //    }
  //
  //    result.toInt;
  //  }

  def intersects(bv: BitVector, position: Int) = {
    (bv.getWord(position) & getWord(position)) != 0;
  }

  def intersects(bv: BitVector): Int = {
    var i = words.length - 1
    while (i >= 0) {
      if (intersects(bv, i)) {
        return i;
      }
      i -= 1
    }

    -1;
  }

  def nbWords = words.length

  def isEmpty: Boolean = {
    for (l <- words) {
      if (l != 0L) {
        return false;
      }
    }
    return true;
  }

  def cardinality = {
    var cardinality = 0;
    for (w <- words) {
      cardinality += java.lang.Long.bitCount(w);
    }
    cardinality;
  }

  def subsetOf(bv: BitVector): Boolean = {
    for (i <- 0 until words.length) {
      if ((words(i) & ~bv.getWord(i)) != 0L) {
        return false;
      }
    }
    true;
  }

  def getWord(i: Int) = {
    if (i >= words.length)
      0L;
    else
      words(i);
  }

  def setWord(pos: Int, word: Long) = {
    val newWords = words.padTo(pos + 1, 0L)
    newWords(pos) = word;
    new LargeBitVector(newWords)
  }

  def filter(f: Int => Boolean) = {
    val words = new Array[Long](nbWords)
    var i = nextSetBit(0)
    while (i >= 0) {
      if (f(i)) {
        words(word(i)) |= (1L << i)
      }
      i = nextSetBit(i + 1)
    }
    if (Arrays.equals(words, this.words)) {
      this
    } else {
      new LargeBitVector(words)
    }
  }

  def getWords: Array[Long] = words

  def lastSetBit: Int = prevSetBit(nbWords * WORD_SIZE)
}
