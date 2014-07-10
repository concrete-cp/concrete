package concrete.constraint.semantic

object SumMode extends Enumeration {
  type SumMode = Value
  val SumLE = Value("le")
  val SumLT = Value("lt")
  val SumEQ = Value("eq")
  //val SumNE = Value("ne")
}