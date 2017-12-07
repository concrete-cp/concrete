package concrete.heuristic.restart

trait RestartStrategy {
  def nextRun(): Option[Int]
  def reset(): Unit
}