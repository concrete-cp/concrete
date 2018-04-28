package concrete


import bitvectors.BitVector

import scala.collection.mutable

class IntDomainBuilder(offset: Int, nbits: Int = BitVector.WORD_SIZE) extends mutable.Builder[Int, IntDomain] {
  private val bitSet = new java.util.BitSet(nbits)

  override def +=(elem: Int): IntDomainBuilder.this.type = {
    bitSet.set(elem - offset)
    this
  }

  def apply(elem: Int): Boolean = bitSet.get(elem - offset)

  override def clear(): Unit = bitSet.clear()

  override def result(): IntDomain = result(bitSet.cardinality())

  /**
    * Result method if cardinality is knowen
    *
    * @param cardinality the cardinality of the domain
    * @return The built domain
    */
  def result(cardinality: Int): IntDomain =
    IntDomain.ofBitVector(offset, BitVector(bitSet), cardinality)
}
