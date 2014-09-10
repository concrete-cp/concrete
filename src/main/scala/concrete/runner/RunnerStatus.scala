package concrete.runner

sealed trait RunnerStatus
case object Sat extends RunnerStatus
case object Unsat extends RunnerStatus
case object Unknown extends RunnerStatus
case object Error extends RunnerStatus