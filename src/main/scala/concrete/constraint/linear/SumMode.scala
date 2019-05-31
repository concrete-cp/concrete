package concrete.constraint.linear

sealed trait SumMode
object SumMode {
  private val map = Seq(LE, LT, EQ, NE, GE, GT)
    .map(o => o.toString -> o)
    .toMap

  def withName(s: String): Option[SumMode] = map.get(s)

  object LE extends SumMode {
    override def toString = "le"
  }

  object LT extends SumMode {
    override def toString = "lt"
  }

  object EQ extends SumMode {
    override def toString = "eq"
  }

  object NE extends SumMode {
    override def toString = "ne"
  }

  object GE extends SumMode {
    override def toString = "ge"
  }

  object GT extends SumMode {
    override def toString = "gt"
  }

}
