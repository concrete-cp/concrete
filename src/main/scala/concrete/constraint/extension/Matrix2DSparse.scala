package concrete.constraint.extension

import bitvectors.BitVector
import com.typesafe.scalalogging.LazyLogging

import scala.collection.mutable

final class Matrix2DSparse(xSize: Int, ySize: Int, val xOffset: Int, val yOffset: Int, initialState: Boolean) extends Matrix with LazyLogging {

  private val defaultY = if (initialState) {
    BitVector.filled(ySize)
  } else {
    BitVector.empty
  }

  private val defaultX = if (initialState) {
    BitVector.filled(xSize)
  } else {
    BitVector.empty
  }

  private var xMatrix = new mutable.HashMap[Int, BitVector]().withDefaultValue(defaultY)

  private var yMatrix = new mutable.HashMap[Int, BitVector]().withDefaultValue(defaultX)
  private var empty = initialState

  def size: Int = xMatrix.size * ySize + yMatrix.size * xSize

  override def check(tuple: Array[Int]): Boolean = xMatrix(tuple(0))(tuple(1) - yOffset)

  override def set(tuple: Seq[Int], status: Boolean): Unit = {
    val Seq(rx, ry) = tuple
    set(rx, ry, status)
  }

  def set(rx: Int, ry: Int, status: Boolean): Unit = {
    val x = rx - xOffset
    val y = ry - yOffset
    require(x >= 0)
    require(y >= 0)

    if (status) {
      xMatrix(rx) += y
      yMatrix(ry) += x
    } else {
      xMatrix(rx) -= y
      yMatrix(ry) -= x
    }
    empty = false

  }

  def getBitVector(position: Int, index: Int): BitVector = {
    assert(0 <= position && position < 2)
    if (position == 0) {
      xMatrix(index)
    } else {
      yMatrix(index)
    }
  }

  def copy: Matrix2DSparse = {
    val matrix2d = new Matrix2DSparse(xSize, ySize, xOffset, yOffset, initialState)
    matrix2d.xMatrix = xMatrix.clone()
    matrix2d.yMatrix = yMatrix.clone()
    matrix2d
  }

  def isEmpty: Boolean = empty

  def allowed: Iterator[Array[Int]] = {
    for {
      (x, ys) <- xMatrix.iterator
      y: Int <- ys.iterator
    } yield {
      Array(x, y + yOffset)
    }
  }

}
