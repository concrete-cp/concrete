package concrete.util;

import java.util.Arrays;
import BitVector._


class LargeBitVector(size:Int, val words:Array[Long]) extends BitVector(size) {

  def this(size:Int) = this(size, Array.ofDim(nbWords(size)))


    
//    
//
//    public void fill(final boolean fill) {
//        if (fill) {
//            Arrays.fill(words, MASK);
//            words[words.length - 1] >>>= -size;
//        } else {
//            Arrays.fill(words, 0);
//        }
//
//    }


  private def updatedWord(wordPos:Int, newWord:Long) = {
    val newWords = words.clone
    newWords(wordPos) = newWord
    new LargeBitVector(size, newWords)
  }

   def clear(position:Int) = {
        val wordPos:Int = word(position)
        val oldWord = words(wordPos)
        val newWord = oldWord & ~(1L << position)
        if (oldWord == newWord) {
          this
        } else {
          updatedWord(wordPos, newWord)
        }
    }

    def set(position:Int) {
      val wordPos = word(position)
      val oldWord = words(wordPos)
      val newWord = oldWord | (1L << position)
      if (oldWord == newWord) {
        this
      } else {
        updatedWord(wordPos, newWord)
      }
    }

    def apply(position:Int)=
         position < size                && (words(word(position)) & (1L << position)) != 0L;
    

    def nextSetBit(start:Int) :Int={
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

    def prevSetBit(start:Int):Int= {

        val wordsInUse = words.length;
        val startWord = BitVector.word(start)
        var position:Int = math.min(wordsInUse - 1, startWord);

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

    def prevClearBit( start:Int):Int= {
        val wordsInUse = words.length;
        val startWord =  BitVector.word(start)
        var position = math.min(wordsInUse - 1,startWord);

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

    def ^(bv:BitVector) =  {
        val (larger, smaller) = 
        if (size < bv.size) {
            (bv, this)
        } else {
          (this,bv)
        }

        var i = larger.nbWords - 1
        val words = Array.ofDim[Long](i)
        words(i) = (larger.getWord(i) ^ smaller.getWord(i)) & (MASK >>> -larger.size)
        i-=1
        while (i >= 0) {
            words(i) = larger.getWord(i) ^ smaller.getWord(i);
            i-=1
        }
        new LargeBitVector(larger.size, words)
    }

   def &(bv:BitVector) {
       val newWords = Array.ofDim[Long](nbWords)
       var i = newWords.length - 1
       newWords(i) = (bv.getWord(i) & words(i)) & (MASK >>> -size);
       i-=1
        while (i >= 0) {
          i-=1
           newWords(i) = bv.getWord(i) & words(i);
        }
        new LargeBitVector(size, newWords)
    }

    def unary_~() = {
        val newWords = Array.ofDim[Long](nbWords)
        var i = words.length - 1;
        newWords(i) = ~words(i) & (MASK >>> -size);
        i-=1
        while (i >= 0) {
          i-=1
            newWords(i) = ~words(i);
        }
        new LargeBitVector(size,newWords)
    }

    def clearFrom(from:Int) = {
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
        var i = newWords.length - 1
        while (i >= startWordIndex+1) {
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

    def setFrom(from:Int) = {
        if (from >= size) {this}
        else {

        val startWordIndex = word(from);

        var changed = false;
        val newWords = words.clone
        val w = newWords(startWordIndex)
        if (startWordIndex == newWords.length - 1) {
            newWords(startWordIndex) |= MASK << from;
            newWords(startWordIndex) &= ~(MASK << size);
            changed |= w != newWords(startWordIndex);
        } else {

            newWords(startWordIndex) |= MASK << from;
            changed |= w != newWords(startWordIndex)

            var i = newWords.length - 2
            
            while (i >= startWordIndex + 1) {
                if (newWords(i) != MASK) {
                    newWords(i) = MASK;
                    changed = true;
                }
            }

            val w2 = newWords(newWords.length - 1);
            newWords(newWords.length - 1) |= (MASK >>> -size);
            changed |= w2 != newWords(newWords.length - 1);
        }

        if (changed) {
          new LargeBitVector(size,newWords)
        } else {
          this
        }

        }
    }

    @Override
    public boolean clearTo(final int to) {
        if (to <= 0) {
            return false;
        }
        int endWordIndex = word(to - 1);

        final long w = words[endWordIndex];
        // Handle first word
        words[endWordIndex] &= ~(MASK >>> -to);
        boolean removed = w != words[endWordIndex];

        // Handle intermediate words, if any
        for (int i = endWordIndex; --i >= 0;) {
            if (words[i] != 0) {
                words[i] = 0;
                removed = true;
            }
        }
        return removed;
    }

    @Override
    public boolean equals(final Object o) {
        final BitVector bv = (BitVector) o;
        final BitVector smaller;
        final BitVector larger;
        if (bv.size < size) {
            smaller = bv;
            larger = this;
        } else {
            smaller = this;
            larger = bv;
        }

        for (int i = nbWords(larger.size); --i >= 0;) {
            if (larger.getWord(i) != smaller.getWord(i)) {
                return false;
            }
        }
        return true;
    }

    public int hashCode() {
        long result = 0;
        for (int i = words.length; --i >= 0;) {
            result = 31 * result + words[i];
        }

        return (int) result;
    }

    @Override
    public void copyTo(final BitVector bv) {
        System.arraycopy(words, 0, ((LargeBitVector) bv).words, 0, words.length);
    }

    @Override
    public boolean intersects(final BitVector bv, final int position) {
        return (bv.getWord(position) & words[position]) != 0;
    }

    @Override
    public int intersects(final BitVector bv) {
        for (int i = words.length; --i >= 0;) {
            if (intersects(bv, i)) {
                return i;
            }
        }

        return -1;
    }

    public int realSize() {
        return words.length;
    }

    @Override
    public boolean isEmpty() {
        for (long l : words) {
            if (l != 0L) {
                return false;
            }
        }
        return true;
    }

    @Override
    public int cardinality() {
        int cardinality = 0;
        for (long w : words) {
            cardinality += Long.bitCount(w);
        }
        return cardinality;
    }

    @Override
    public BitVector clone() throws CloneNotSupportedException {
        final LargeBitVector bv = (LargeBitVector) super.clone();
        bv.words = words.clone();
        return bv;
    }

    public boolean subsetOf(BitVector bv) {
        for (int i = words.length; --i >= 0;) {
            if ((words[i] & ~bv.getWord(i)) != 0L) {
                return false;
            }
        }
        return true;
    }

    public long getWord(int i) {
        if (i >= words.length)
            return 0L;
        else
            return words[i];
    }

    @Override
    protected void setFirstWord(long word) {
        words[0] = word;

    }
}
