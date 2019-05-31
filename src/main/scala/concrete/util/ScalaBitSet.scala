package concrete.util

import java.util

import scala.collection.{SortedSetLike, mutable}

final class ScalaBitSet(private val bitSet: util.BitSet = new util.BitSet()) extends mutable.SortedSet[Int] with SortedSetLike[Int, ScalaBitSet]
  with mutable.SetLike[Int, ScalaBitSet] {

  override def empty = new ScalaBitSet()

  def this(initSize: Int) = this(new util.BitSet(initSize))

  def iterator: Iterator[Int] = keysIteratorFrom(0)

  override def keysIteratorFrom(start: Int): Iterator[Int] = new Iterator[Int] {
    private var c = bitSet.nextSetBit(start)

    def hasNext: Boolean = c >= 0

    def next: Int = {
      val r = c
      c = bitSet.nextSetBit(c + 1)
      r
    }
  }

  def contains(i: Int): Boolean = bitSet.get(i)

  def -=(i: Int): ScalaBitSet.this.type = {
    bitSet.clear(i)
    this
  }

  def +=(i: Int): ScalaBitSet.this.type = {
    bitSet.set(i)
    this
  }

  def set(from: Int, until: Int): Unit = bitSet.set(from, until)

  implicit def ordering: Ordering[Int] = Ordering.Int

  override def clear(): Unit = bitSet.clear()

  def rangeImpl(from: Option[Int], until: Option[Int]): ScalaBitSet = {
    val bs = bitSet.clone().asInstanceOf[util.BitSet]
    for (lb <- from) bs.clear(0, lb)
    for (ub <- until) bs.clear(ub, bs.length())
    new ScalaBitSet(bs)
  }

  def next(from: Int): Int = bitSet.nextSetBit(from)

  override def size: Int = bitSet.cardinality()
}
