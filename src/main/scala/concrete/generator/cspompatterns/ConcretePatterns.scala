package concrete.generator.cspompatterns

import cspom.compiler.MergeEq
import cspom.compiler.MergeSame
import cspom.compiler.StandardCompilers

object ConcretePatterns {
  def apply() =
    StandardCompilers() ++
      Seq(
        AbsDiff, SubToAdd, AddToEq,
        SubsumedDiff, AllDiff, RemoveUselessEq, DiffGe,
        UnaryOr, MergeDisj, MergeNotDisj,
        NegToCNF, ReifiedDisj, ReifiedConj,
        SimplDisj,
        NeqVec, RemoveAnd, SplitAllEq,
        LtToGt //, ConstToVar
        )

}