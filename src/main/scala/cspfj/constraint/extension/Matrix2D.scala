package cspfj.constraint.extension;

import cspfj.util.BitVector;

final class Matrix2D(xSize: Int, ySize: Int, initialState: Boolean) extends Matrix {
  private var xMatrix =
    new Array[BitVector](xSize).map(_ => {
      val bv = BitVector.newBitVector(ySize)
      if (initialState) bv.fill(true)
      bv
    })

  private var yMatrix =
    new Array[BitVector](ySize).map(_ => {
      val bv = BitVector.newBitVector(xSize)
      if (initialState) bv.fill(true)
      bv
    })

  private var empty = initialState

  override def check(tuple: Array[Int]) = xMatrix(tuple(0)).get(tuple(1))

  override def set(tuple: Array[Int], status: Boolean) {
    xMatrix(tuple(0)).set(tuple(1), status);
    yMatrix(tuple(1)).set(tuple(0), status);
    empty = false;
  }

  def getBitVector(position: Int, index: Int) = position match {
    case 0 => xMatrix(index);
    case 1 => yMatrix(index);
    case _ => throw new IllegalArgumentException;
  }

  def copy = {
    val matrix2d = new Matrix2D(xSize, ySize, initialState)
    matrix2d.xMatrix = xMatrix map (_.clone)
    matrix2d.yMatrix = yMatrix map (_.clone)
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
