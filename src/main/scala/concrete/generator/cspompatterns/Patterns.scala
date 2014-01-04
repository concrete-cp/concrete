package concrete.generator.cspompatterns

object Patterns {
  def apply() =
    Seq(
      AbsDiff, SubToAdd, AddToEq,
      SubsumedDiff, AllDiff, RemoveUselessEq, DiffGe,
      UnaryOr, MergeDisj, MergeNotDisj, MergeEq,
      NegToCNF, ReifiedDisj, ReifiedConj,
      SimplDisj, MergeSame,
      NeqVec, RemoveAnd, SplitAllEq,
      LtToGt //, ConstToVar
      )

}