package concrete.constraint.extension;

trait Matrix {

  def set(tuple: Array[Int], status: Boolean)

  def setAll(tuple: Traversable[Array[Int]], status: Boolean) = {
    for (t <- tuple) set(t, status)
    this
  }

  def check(tuple: Array[Int]): Boolean

  def copy: Matrix

  def isEmpty: Boolean

}
