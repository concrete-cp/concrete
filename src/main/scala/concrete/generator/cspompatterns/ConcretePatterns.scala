package concrete.generator.cspompatterns

import concrete.ParameterManager
import cspom.compiler.StandardCompilers
import cspom.compiler.MergeSame
import cspom.compiler.MergeEq
import cspom.compiler.RemoveUselessEq
import cspom.compiler.SplitEqVec

object ConcretePatterns {

  def apply(params: ParameterManager) = {
    val concreteDef =
      Seq(
        SubToAdd, DivToMul, AbsDiff, AbsDiffDomains, AddToEq, BoolEq,
        AddDomains, MulDomains, EqDomains, AbsDomains, NeqToEq,
        Bool2IntDomains, MulToSum, SumFactors, SumDomains,
        MergeNotDisj, DisjToClause,
        NegToCNF, Xor, ReifiedConj, ReifiedClause,
        NeqVec, SimplClause,
        LtToGt, ReversedGt, SlidingSum, Regular, SetIn, ConcreteTypes,
        SumNe, AllDifferent, AllDiffConstant, CountEq //, ConstToVar
        )

    val concreteImp = Seq(
      AllDiff, SubsumedDiff, DiffGe, Square, GtDomains, GeDomains, UnaryClause)

    val improveModel = params.getOrElse("improveModel", true)

    if (improveModel) {
      StandardCompilers() ++ StandardCompilers.improve() ++ concreteDef ++ concreteImp
    } else {
      StandardCompilers() ++ concreteDef
    }
        
        

  }

}