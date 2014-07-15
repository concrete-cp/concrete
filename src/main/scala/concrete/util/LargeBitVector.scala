package concrete.util;

import java.util.Arrays;
import BitVector._

class LargeBitVector(size: Int, val words: Array[Long]) extends BitVector(size) {

  def this(size: Int) = this(size, Array.ofDim(nbWords(size)))

  private def updatedWord(wordPos: Int, newWord: Long) = {
    val newWords = words.clone
    newWords(wordPos) = newWord
    new LargeBitVector(size, newWords)
  }

  def -(position: Int) = {
    val wordPos: Int = word(position)
    val oldWord = words(wordPos)
    val newWord = oldWord & ~(1L << position)
    if (oldWord == newWord) {
      this
    } else {
      updatedWord(wordPos, newWord)
    }
  }

  def +(position: Int) = {
    val wordPos = word(position)
    val oldWord = words(wordPos)
    val newWord = oldWord | (1L << position)
    if (oldWord == newWord) {
      this
    } else {
      updatedWord(wordPos, newWord)
    }
  }

  def apply(position: Int) =
    position < size && (words(word(position)) & (1L << position)) != 0L;

  def nextSetBit(start: Int): Int = {
    var position = word(start);
    val wordsInUse = words.length;
    if (position >= wordsInUse) {
      -1
    } else {
      var word = words(position) & (MASK << start);

      while (word == 0) {
        position += 1
        if (position == wordsInUse) {
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
    val (larger, smaller) =
      if (size < bv.size) {
        (bv, this)
      } else {
        (this, bv)
      }

    var i = larger.nbWords - 1
    val words = Array.ofDim[Long](i)
    words(i) = (larger.getWord(i) ^ smaller.getWord(i)) & (MASK >>> -larger.size)
    i -= 1
    while (i >= 0) {
      words(i) = larger.getWord(i) ^ smaller.getWord(i);
      i -= 1
    }
    new LargeBitVector(larger.size, words)
  }

  def &(bv: BitVector) {
    val newWords = Array.ofDim[Long](nbWords)
    var i = newWords.length - 1
    newWords(i) = (bv.getWord(i) & words(i)) & (MASK >>> -size);
    i -= 1
    while (i >= 0) {
      i -= 1
      newWords(i) = bv.getWord(i) & words(i);
    }
    new LargeBitVector(size, newWords)
  }

  def unary_~() = {
    val newWords = Array.ofDim[Long](nbWords)
    var i = words.length - 1;
    newWords(i) = ~words(i) & (MASK >>> -size);
    i -= 1
    while (i >= 0) {
      i -= 1
      newWords(i) = ~words(i);
    }
    new LargeBitVector(size, newWords)
  }

  def clearFrom(from: Int) = {
    if (from >= size) {
      this
    } else {

      val startWordIndex = word(from);

      val newWords = words.clone

      val w = newWords(startWordIndex);
      // Handle first word
      newWords(startWordIndex) &= ~(MASK << from);
      var removed = w != newWords(startWordIndex)
      // Handle intermediate words, if any
      for (i <- startWordIndex + 1 until newWords.length) {
        if (newWords(i) != 0) {
          newWords(i) = 0
          removed = true
        }
      }

      if (removed) {
        new LargeBitVector(size, newWords)
      } else {
        this
      }

    }
  }

  def setFrom(from: Int) = {
    if (from >= size) { this }
    else {

      val startWordIndex = word(from);

      val newWords = words.clone

      // Handle first word
      val w = newWords(startWordIndex)
      newWords(startWordIndex) |= MASK << from;

      var changed = w != newWords(startWordIndex);
      // Handle intermediate words, if any
      for (i <- startWordIndex + 1 until newWords.length - 1) {
        if (newWords(i) != MASK) {
          newWords(i) = MASK
          changed = true
        }
      }

      // Handle last word
      val w2 = newWords(newWords.length - 1);

      newWords(newWords.length - 1) |= truncate(MASK, size)
      changed |= w2 != newWords(newWords.length - 1);

      if (changed) {
        new LargeBitVector(size, newWords)
      } else {
        this
      }

    }
  }

  def clearTo(to: Int) = {
    if (to <= 0) {
      this
    } else {
      val endWordIndex = word(to - 1);
      val newWords = words.clone
      val w = newWords(endWordIndex)
      // Handle first word
      words(endWordIndex) &= ~(MASK >>> -to);
      var removed = w != newWords(endWordIndex);

      // Handle intermediate words, if any
      for (i <- 0 until endWordIndex) {
        if (newWords(i) != 0) {
          newWords(i) = 0;
          removed = true;
        }
      }

      if (removed) {
        new LargeBitVector(size, newWords)
      } else {
        this
      }
    }
  }

  override def equals(o: Any): Boolean = o match {
    case bv: BitVector =>
      for (i <- 0 until nbWords) {
        if (getWord(i) != bv.getWord(i)) {
          return false;
        }
      }
      true;
    case _ => false
  }

  override def hashCode(): Int = {
    var result = 721L;
    for (w <- words) {
      result = 31 * result + w;
    }

    result.toInt;
  }

  def intersects(bv: BitVector, position: Int) = {
    (bv.getWord(position) & words(position)) != 0;
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

  def isEmpty(): Boolean = {
    for (l <- words) {
      if (l != 0L) {
        return false;
      }
    }
    return true;
  }

  def cardinality() {
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

  def setFirstWord(word: Long) = {
    val newWords = words.clone
    newWords(0) = word;
    new LargeBitVector(size, newWords)
  }
}
