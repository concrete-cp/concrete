package concrete.runner

sealed trait Result

case object FullExplore extends Result

object Error {
  def apply(t: Throwable) = Unfinished(Some(t))
}

case class Unfinished(t: Option[Throwable] = None) extends Result