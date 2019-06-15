package concrete.constraint.extension

import bitvectors.BitVector
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

  private var empty = initialState

  def size: Int = xMatrix.length * yMatrix.length

  override def set(tuple: Array[Int], status: Boolean): Unit = {
    val Array(rx, ry) = tuple
    set(rx, ry, status)
  }

  def set(rx: Int, ry: Int, status: Boolean): Unit = {
    val x = rx - xOffset
    val y = ry - yOffset
    require(x >= 0)
    require(y >= 0)
    require(x < xMatrix.length, s"Tuple ($rx, $ry) is out of the scope of matrix $this")
    require(y < yMatrix.length, s"Tuple ($rx, $ry) is out of the scope of matrix $this")
    //if (x < xMatrix.length && y < yMatrix.length) {
    if (status) {
      xMatrix(x) += y
      yMatrix(y) += x
    } else {
      xMatrix(x) -= y
      yMatrix(y) -= x
    }
    empty = false
    //}

    //    else {
    //      logger.info(s"Tuple ($rx, $ry) is out of the scope of matrix $this")
    //    }

  }

  def offsets(position: Int):Int = {
    assert(0 <= position && position < 2)
    if (position == 0) {
      xOffset
    } else {
      yOffset
    }
  }

  def getBitVector(position: Int, index: Int): BitVector = {
    assert(0 <= position && position < 2)
    if (position == 0) {
      xMatrix(index - xOffset)
    } else {
      yMatrix(index - yOffset)
    }
  }

  def copy: Matrix2D = {
    val matrix2d = new Matrix2D(xSize, ySize, xOffset, yOffset, initialState)
    matrix2d.xMatrix = xMatrix.clone
    matrix2d.yMatrix = yMatrix.clone
    matrix2d
  }

  override def toString: String = {
    val stb = new StringBuilder
    stb.append(s"x offset: $xOffset, y offset: $yOffset\n")
    for (i <- xMatrix.indices) {
      for (j <- yMatrix.indices) {
        if (xMatrix(i)(j)) {
          stb.append(1)
        } else {
          stb.append(0)
        }
      }
      stb.append('\n')
    }
    stb.toString
  }

  override def check(tuple: Array[Int]):Boolean = xMatrix(tuple(0) - xOffset)(tuple(1) - yOffset)

  def isEmpty: Boolean = empty

  def allowed: Iterator[Array[Int]] = {
    for {
      x <- (0 until xSize).iterator
      y <- xMatrix(x).iterator
    } yield {
      Array(x + xOffset, y + yOffset)
    }
  }

}
