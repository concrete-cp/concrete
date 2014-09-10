package concrete.util

import scala.collection.SortedSet
import scala.collection.mutable.ArrayBuffer

object Indexer {
  def factory(values: SortedSet[Int]): Indexer = {
    val ub = values.last
    val lb = values.head
    if (values.size == ub - lb + 1) { ofInterval(lb, ub) }
    else { new GeneralIndices(values.toArray) }
  }

  def ofInterval(lb: Int, ub: Int) =
    if (lb == 0) { new DirectIndices(ub - lb + 1) }
    else { new OffsetIndices(ub - lb + 1, lb) }

}

trait Indexer {
  def index(value: Int): Int
  def value(index: Int): Int
  def contiguous: Boolean
}

case class Entry(value: Int, index: Int)

final class GeneralIndices(val values: Array[Int]) extends Indexer {
  //private val indicesMap = values.zipWithIndex.map { case (v, i) => v -> i }.toMap.withDefaultValue(-1)

  require(values.length <= (1 << 30))

  private val size = {
    var c = 1
    while (c < values.length * 2) {
      c <<= 1
    }
    c
  }

  private val entries = {
    val array = new Array[ArrayBuffer[Entry]](size)

    for ((v, i) <- values.zipWithIndex) {
      val idx = entry(v)

      val buffer = array(idx) match {
        case null =>
          val buffer = new ArrayBuffer[Entry]
          array(idx) = buffer
          buffer
        case b => b
      }
      buffer += Entry(v, i)
    }

    array.map {
      case a: ArrayBuffer[Entry] => a.toArray
      case null => null
    }
  }

  def entry(value: Int) = value & (size - 1)

  def index(value: Int): Int = {
    val array = entries(entry(value))
    if (array != null) {
      var i = array.length - 1
      while (i >= 0) {
        val a = array(i)
        if (a.value == value) {
          return a.index
        }
        i -= 1
      }
    }
    -1
  }
  def value(index: Int) = values(index)
  def contiguous = false
}

final class OffsetIndices(val size: Int, val offset: Int) extends Indexer {
  def index(value: Int) = {
    val i = value - offset
    if (i < 0 || i >= size) -1 else i
  }
  def value(index: Int) = index + offset
  def contiguous = true

  override def toString = "[0, %d] + %d".format(size - 1, offset)
}

final class DirectIndices(val size: Int) extends Indexer {
  def index(value: Int) = if (value < 0 || value >= size) -1 else value
  def value(index: Int) = index
  def contiguous = true
}
