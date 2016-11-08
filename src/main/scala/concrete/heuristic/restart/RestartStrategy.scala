package concrete.heuristic.restart

trait RestartStrategy {
  def nextRun(): Int
  def reset(): Unit
}