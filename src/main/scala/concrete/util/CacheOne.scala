package concrete.util

class CacheOne[A, B >: Null] {
  var key: A = _
  var value: B = null

  def apply(k: A, f: => B) = {
    if ((null != value) && k == key) value else {
      value = f
      key = k
      value
    }
  }

}