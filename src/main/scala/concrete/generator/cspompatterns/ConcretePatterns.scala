package concrete.generator.cspompatterns

import cspom.compiler.MergeEq
import cspom.compiler.MergeSame
import cspom.compiler.StandardCompilers
import cspom.compiler.RemoveUselessEq
import cspom.compiler.SplitAllEq

object ConcretePatterns {
  def apply() =
    StandardCompilers() ++
      Seq(
        AbsDiff, SubToAdd, AddToEq,
        SubsumedDiff, AllDiff, DiffGe,
        UnaryOr, MergeDisj, MergeNotDisj,
        NegToCNF, ReifiedDisj, ReifiedConj,
        SimplDisj,
        NeqVec, RemoveAnd,
        LtToGt //, ConstToVar
        )

}