package cspfj.util

object Indexer {
  def factory(values: Seq[Int]): Indexer = {
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

  override def toString = "[0, %d] + %d".format(size - 1, offset)
}

final class DirectIndices(val size: Int) extends Indexer {
  def index(value: Int) = if (value < 0 || value >= size) -1 else value
  def value(index: Int) = index
}
