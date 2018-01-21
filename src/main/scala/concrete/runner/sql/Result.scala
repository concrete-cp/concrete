package concrete.runner.sql

sealed trait CompetResult {
  def solved: Boolean

  def optimal: Boolean

  def result: Option[Int]
}

case object SAT extends CompetResult {
  override def toString: String = "SAT1"
  def result = None
  def solved = true
  def optimal = false
}

case object UNSAT extends CompetResult {
  override def toString: String = "UNSAT"
  def result = None
  def solved = true
  def optimal = true
}

case class Optimal(opt: Int) extends CompetResult {
  override def toString: String = s"$opt*"
  def solved = true
  def optimal = true
  def result = Some(opt)
}

case class SubOptimal(found: Int) extends CompetResult {
  override def toString: String = Integer.toString(found)
  def solved = true
  def optimal = false
  def result = Some(found)
}

case object Unknown extends CompetResult {
  def solved = false
  def optimal = false
  def result = None
}