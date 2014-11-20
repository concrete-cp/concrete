package concrete.constraint.extension;

import concrete.util.BitVector;
import com.typesafe.scalalogging.LazyLogging

final class Matrix1D(mSize: Int, initialState: Boolean) extends Matrix with LazyLogging {
  private var set = if (initialState) {
    BitVector.filled(mSize)
  } else {
    BitVector.empty
  }

  def size: Int = mSize

  private var empty = initialState

  override def check(tuple: Array[Int]) = set(tuple(0))

  override def set(tuple: Seq[Int], status: Boolean) {
    val Seq(v) = tuple
    if (v < mSize) {
      set = set.set(v, status);
      empty = false;
    } else {
      logger.info(s"Tuple ${tuple.mkString("(", ", ", ")")} is out of the scope of matrix $this")
    }

  }

  def copy = {
    val matrix1d = new Matrix1D(mSize, initialState)
    matrix1d.set = set
    matrix1d
  }

  override def toString = (0 until mSize)
    .map { i =>
      if (set(i)) { 1 } else { 0 }
    }
    .mkString

  def isEmpty = empty

}
