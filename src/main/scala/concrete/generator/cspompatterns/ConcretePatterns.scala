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
        UnaryOr, MergeDisj, MergeNotDisj,
        NegToCNF, ReifiedDisj, ReifiedConj,
        NeqVec, RemoveAnd,
        LtToGt, SlidingSum, SetIn, Element, In, MinMax//, //SimplDisj//ConstToVar
        )

    if (improveModel) {
      StandardCompilers.improve() ++ Seq(AllDiff, SubsumedDiff, DiffGe, SimplDisj) ++ standard
    } else {
      standard
    }
  }

}