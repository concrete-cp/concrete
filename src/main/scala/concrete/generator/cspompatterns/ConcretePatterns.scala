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
        SubToAdd, AbsDiff, AbsDiffDomains, AddToEq, BoolEq,
        AddDomains, EqDomains, AbsDomains, EqToNeq,
        Bool2IntDomains, MulToSum, SumFactors, SumDomains,
        UnaryOr, MergeNotDisj,
        NegToCNF, Xor, ReifiedDisj, ReifiedConj,
        NeqVec, SimplDisj,
        LtToGt, SlidingSum, Regular, SetIn, In, MinMax, ConcreteTypes,
        AllDiffConstant //, //SimplDisj//ConstToVar
        )

    val improveModel = params.getOrElse("improveModel", true)

    if (improveModel) {
      StandardCompilers.improve() ++ standard ++ Seq(
        AllDiff, SubsumedDiff, DiffGe, Square, GtDomains, GeDomains)
    } else {
      standard
    }
  }

}