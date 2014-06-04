package concrete.generator.cspompatterns

import cspom.compiler.MergeEq
import cspom.compiler.MergeSame
import cspom.compiler.StandardCompilers
import cspom.compiler.RemoveUselessEq
import cspom.compiler.SplitAllEq
import concrete.ParameterManager

object ConcretePatterns {

  def apply(params: ParameterManager) = {
    val standard = StandardCompilers() ++
      Seq(
        AbsDiff, AbsDiffDomains, SubToAdd, AddToEq, BoolEq,
        AddDomains, EqDomains,
        UnaryOr, MergeDisj, MergeNotDisj,
        NegToCNF, Xor, ReifiedDisj, ReifiedConj,
        NeqVec,
        LtToGt, SlidingSum, SetIn, Element, In, MinMax //, //SimplDisj//ConstToVar
        )

    val improveModel = params.getOrElse("improveModel", true)

    if (improveModel) {
      standard ++ StandardCompilers.improve() ++ Seq(AllDiff, SubsumedDiff, DiffGe, SimplDisj, Square, GtDomains)
    } else {
      standard
    }
  }

}