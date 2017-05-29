package concrete.util

import scala.util.Try

object TryWith {
  def apply[T <: AutoCloseable, R](resGen: => T)(r: T => R): Try[R] = {

    Try(resGen).flatMap { closeable =>

      val res = Try(r(closeable))

      Try(closeable.close())
        .flatMap(_ => res)

    }
  }
}
