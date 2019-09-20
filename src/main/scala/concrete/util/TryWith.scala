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
//
//object TryFlatten {
//  def apply[T](xs: Seq[Try[T]]): Try[Seq[T]] = {
//    val (failures, successes) = xs.partitionMap(_.toEither)
//    failures.headOption
//      .map(Failure(_))
//      .getOrElse(Success(successes))
//  }
//}