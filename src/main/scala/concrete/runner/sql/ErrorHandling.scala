package concrete.runner.sql

sealed trait ErrorHandling {
  def toDouble(stat: Option[Double]): Double
}
case object ErrorKeep extends ErrorHandling {
  def toDouble(stat: Option[Double]) = stat.get
}
case object ErrorNaN extends ErrorHandling {
  def toDouble(stat: Option[Double]) = Double.NaN
}
case class ErrorCap(cap: Double) extends ErrorHandling {
  def toDouble(stat: Option[Double]) = cap
}

