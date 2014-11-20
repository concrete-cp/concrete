package concrete.constraint.extension;

import concrete.util.BitVector;
import com.typesafe.scalalogging.LazyLogging

final class Matrix2D(xSize: Int, ySize: Int, initialState: Boolean) extends Matrix with LazyLogging {
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

  def size: Int = xMatrix.size * yMatrix.size

  private var empty = initialState

  override def check(tuple: Array[Int]) = xMatrix(tuple(0))(tuple(1))

  override def set(tuple: Seq[Int], status: Boolean) {
    val Seq(x, y) = tuple
    require(x >= 0)
    require(y >= 0)
    if (x < xMatrix.length && y < yMatrix.length) {
      xMatrix(x) = xMatrix(x).set(y, status);
      yMatrix(y) = yMatrix(y).set(x, status);
      empty = false;
    } else {
      logger.info(s"Tuple ${tuple.mkString("(", ", ", ")")} is out of the scope of matrix $this")
    }

  }

  def getBitVector(position: Int, index: Int) = position match {
    case 0 => xMatrix(index);
    case 1 => yMatrix(index);
    case _ => throw new IllegalArgumentException;
  }

  def copy = {
    val matrix2d = new Matrix2D(xSize, ySize, initialState)
    matrix2d.xMatrix = xMatrix.clone
    matrix2d.yMatrix = yMatrix.clone
    matrix2d;
  }

  override def toString = {
    val stb = new StringBuilder;
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

}
