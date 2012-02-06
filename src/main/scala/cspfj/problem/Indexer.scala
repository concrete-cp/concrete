package cspfj.problem

object Indexer {
  def factory(values: Array[Int]): Indexer = {
    if (values.size == values.last - values.head + 1) {
      if (values.first == 0) new DirectIndices(values.size)
      else new OffsetIndices(values.size, values.head)
    } else new GeneralIndices(values)
  }
}

trait Indexer {
  def index(value: Int): Int
  def value(index: Int): Int
}

final class GeneralIndices(val values: Array[Int]) extends Indexer {
  private val indicesMap = values.zipWithIndex.map { case (v, i) => v -> i }.toMap.withDefaultValue(-1)

  def index(value: Int) = indicesMap(value)
  def value(index: Int) = values(index)
}

final class OffsetIndices(val size: Int, val offset: Int) extends Indexer {
  def index(value: Int) = {
    val i = value - offset
    if (i < 0 || i >= size) -1 else i
  }
  def value(index: Int) = index + offset

}

final class DirectIndices(val size: Int) extends Indexer {
  def index(value: Int) = if (value < 0 || value >= size) -1 else value
  def value(index: Int) = index
}