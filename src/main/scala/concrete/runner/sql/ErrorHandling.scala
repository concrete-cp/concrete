package concrete.runner.sql

sealed trait ErrorHandling {
  def toDouble(stat: Double): Double
}
case object ErrorKeep extends ErrorHandling {
  def toDouble(stat: Double) = stat
}
case object ErrorNaN extends ErrorHandling {
  def toDouble(stat: Double) = Double.NaN
}
case class ErrorCap(cap: Double) extends ErrorHandling {
  def toDouble(stat: Double) = cap
}

