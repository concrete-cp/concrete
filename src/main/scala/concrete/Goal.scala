package concrete

sealed trait Goal {
  def optimizes: Option[Variable]
}

case object Satisfy extends Goal {
  def optimizes = None
}

case class Minimize(v: Variable) extends Goal {
  def optimizes = Some(v)
}

case class Maximize(v: Variable) extends Goal {
  def optimizes = Some(v)
}
