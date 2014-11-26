package concrete.util

object Math {
  /**
   * Returns the product of {@code a} and {@code b}, provided it does not overflow.
   *
   * @throws ArithmeticException if {@code a * b} overflows in signed {@code int} arithmetic
   */
  def checkedMultiply(a: Int, b: Int) {
    val result = a.toLong * b;
    if (result != result.toInt) throw new ArithmeticException("overflow")
    result.toInt;
  }

  /**
   * Returns the sum of {@code a} and {@code b}, provided it does not overflow.
   *
   * @throws ArithmeticException if {@code a + b} overflows in signed {@code int} arithmetic
   */
  def checkedAdd(a: Int, b: Int) {
    val result = a.toLong + b;
    if (result != result.toInt) throw new ArithmeticException("overflow")
    result.toInt;
  }
}