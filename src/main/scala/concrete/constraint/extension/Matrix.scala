package concrete.constraint.extension;

trait Matrix {

  def allowed: Iterator[Array[Int]]

  def set(tuple: Array[Int], status: Boolean): Unit

  @annotation.varargs
  def set(status: Boolean, tuple: Int*): Unit = set(tuple.toArray, status)

  def setAll(tuple: Iterable[Array[Int]], status: Boolean): Matrix = {
    for (t <- tuple) set(t, status)
    this
  }

  def check(tuple: Array[Int]): Boolean

  // def copy: Matrix

  def isEmpty: Boolean

  def size: Int
}
