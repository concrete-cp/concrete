package concrete.generator.cspompatterns

import cspom.compiler.MergeEq
import cspom.compiler.MergeSame
import cspom.compiler.StandardCompilers
import cspom.compiler.RemoveUselessEq
import cspom.compiler.SplitAllEq
import concrete.ParameterManager

object ConcretePatterns {

  def apply(params: ParameterManager) = {
    val concreteDef =
      Seq(
        SubToAdd, AbsDiff, AbsDiffDomains, AddToEq, BoolEq,
        AddDomains, MulDomains, EqDomains, AbsDomains, NeqToEq,
        Bool2IntDomains, MulToSum, SumFactors, SumDomains,
        UnaryClause, MergeNotDisj, DisjToClause,
        NegToCNF, Xor, ReifiedClause, ReifiedConj,
        NeqVec, SimplClause,
        LtToGt, SlidingSum, Regular, SetIn, In, MinMax, ConcreteTypes,
        AllDiffConstant //, ConstToVar
        )

    val concreteImp = Seq(
      AllDiff, SubsumedDiff, DiffGe, Square, GtDomains, GeDomains)

    val improveModel = params.getOrElse("improveModel", true)

    if (improveModel) {
      StandardCompilers() ++ StandardCompilers.improve() ++ concreteDef ++ concreteImp
    } else {
      StandardCompilers() ++ concreteDef
    }
  }

}