package concrete.generator.cspompatterns

import cspom.compiler.MergeEq
import cspom.compiler.MergeSame
import cspom.compiler.StandardCompilers
import cspom.compiler.RemoveUselessEq
import cspom.compiler.SplitAllEq
import concrete.Parameter

object ConcretePatterns {

  @Parameter("improveModel")
  var improveModel = true

  def apply() = {
    val standard = StandardCompilers() ++
      Seq(
        AbsDiff, SubToAdd, AddToEq,
        SubsumedDiff, 
        UnaryOr, MergeDisj, MergeNotDisj,
        NegToCNF, ReifiedDisj, ReifiedConj,
        SimplDisj,
        NeqVec, RemoveAnd,
        LtToGt, SlidingSum, SetIn //, ConstToVar
        )

    if (improveModel) {
      StandardCompilers.improve() ++ Seq(AllDiff, DiffGe) ++ standard
    } else {
      standard
    }
  }

}