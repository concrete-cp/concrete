package concrete.runner

sealed trait RunnerResult

case object FullExplore extends RunnerResult

object Error {
  def apply(t: Throwable) = Unfinished(Some(t))
}

case class Unfinished(t: Option[Throwable] = None) extends RunnerResult