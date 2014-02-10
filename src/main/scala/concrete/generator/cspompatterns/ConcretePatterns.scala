package concrete.generator.cspompatterns

object ConcretePatterns {
  def apply() =
    Seq(MergeSame,
      AbsDiff, SubToAdd, AddToEq,
      SubsumedDiff, AllDiff, RemoveUselessEq, DiffGe,
      UnaryOr, MergeDisj, MergeNotDisj, MergeEq,
      NegToCNF, ReifiedDisj, ReifiedConj,
      SimplDisj,
      NeqVec, RemoveAnd, SplitAllEq,
      LtToGt //, ConstToVar
      )

}