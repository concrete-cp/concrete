package concrete.constraint.extension;

import bitvectors.BitVector;
import com.typesafe.scalalogging.LazyLogging

final class Matrix2D(xSize: Int, ySize: Int, val xOffset: Int, val yOffset: Int, initialState: Boolean) extends Matrix with LazyLogging {
  private var xMatrix = {
    val bv = if (initialState) {
      BitVector.filled(ySize)
    } else {
      BitVector.empty
    }
    Array.fill(xSize)(bv)
  }

  private var yMatrix = {
    val bv = if (initialState) {
      BitVector.filled(xSize)
    } else {
      BitVector.empty
    }
    Array.fill(ySize)(bv)
  }

  def size: Int = xMatrix.length * yMatrix.length

  private var empty = initialState

  override def check(tuple: Array[Int]) = xMatrix(tuple(0) - xOffset)(tuple(1) - yOffset)

  override def set(tuple: Seq[Int], status: Boolean): Unit = {
    val Seq(rx, ry) = tuple
    set(rx, ry, status)
  }

  def set(rx: Int, ry: Int, status: Boolean): Unit = {
    val x = rx - xOffset
    val y = ry - yOffset
    require(x >= 0)
    require(y >= 0)
    if (x < xMatrix.length && y < yMatrix.length) {
      if (status) {
        xMatrix(x) += y
        yMatrix(y) += x
      } else {
        xMatrix(x) -= y
        yMatrix(y) -= x
      }
      empty = false;
    } else {
      logger.info(s"Tuple ($rx, $ry) is out of the scope of matrix $this")
    }

  }

  def offsets(position: Int) = {
    assert(0 <= position && position < 2)
    if (position == 0) {
      xOffset
    } else {
      yOffset
    }
  }

  def getBitVector(position: Int, index: Int) = {
    assert(0 <= position && position < 2)
    if (position == 0) {
      xMatrix(index - xOffset)
    } else {
      yMatrix(index - yOffset)
    }
  }

  def copy = {
    val matrix2d = new Matrix2D(xSize, ySize, xOffset, yOffset, initialState)
    matrix2d.xMatrix = xMatrix.clone
    matrix2d.yMatrix = yMatrix.clone
    matrix2d;
  }

  override def toString = {
    val stb = new StringBuilder;
    stb.append(s"x offset: $xOffset, y offset: $yOffset\n")
    for (i <- 0 until xMatrix.length) {
      for (j <- 0 until yMatrix.length) {
        if (check(Array(i, j))) {
          stb.append(1)
        } else {
          stb.append(0)
        }
      }
      stb.append('\n')
    }
    stb.toString
  }

  def isEmpty = empty

  def allowed: Iterator[Array[Int]] = {
    for {
      x <- (0 until xSize).iterator
      y <- xMatrix(x).iterator
    } yield {
      Array(x + xOffset, y + yOffset)
    }
  }

}
