package concrete.util

import java.util

class BitSetQueue(private val set: util.BitSet = new util.BitSet) {

  def this(words: Array[Long]) = this(util.BitSet.valueOf(words))

  var i: Int = 0

  def isEmpty: Boolean = set.isEmpty

  def offer(e: Int): Unit = set.set(e)

  def poll(): Int = {
    var next = set.nextSetBit(i)
    if (next < 0) next = set.nextSetBit(0)
    if (next < 0) throw new NoSuchElementException
    set.clear(next)
    i = next + 1
    next
  }
}
