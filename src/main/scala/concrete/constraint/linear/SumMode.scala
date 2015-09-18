package concrete.constraint.linear

sealed trait SumMode
object SumMode {
  val map = Seq(SumLE, SumLT, SumEQ, SumNE)
    .map(o => o.toString -> o)
    .toMap

  def withName(s: String): Option[SumMode] = map.get(s)
}
object SumLE extends SumMode {
  override def toString = "le"
}
object SumLT extends SumMode {
  override def toString = "lt"
}
object SumEQ extends SumMode {
  override def toString = "eq"
}
object SumNE extends SumMode {
  override def toString = "ne"
}