package concrete.util

class CacheOne[A, B >: Null] {
  var key: A = _
  var value: B = _

  def apply(k: A, f: => B): B = {
    if ((null != value) && k == key) value else {
      value = f
      key = k
      value
    }
  }

}