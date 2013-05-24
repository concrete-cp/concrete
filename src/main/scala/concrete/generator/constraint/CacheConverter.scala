package concrete.generator.constraint

final class CacheConverter[A <: AnyRef, B] {

  var cache: List[(A, B)] = Nil

  def getOrAdd(k: A, f: => B) = {
    cache.find(k eq _._1) match {
      case Some(b) => b._2
      case None => {
        val b = f
        cache ::= (k, b)
        b
      }
    }
  }

}