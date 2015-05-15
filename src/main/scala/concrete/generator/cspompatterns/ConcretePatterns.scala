package concrete.generator.cspompatterns

import concrete.ParameterManager
import cspom.compiler.StandardCompilers
import cspom.compiler.MergeSame
import cspom.compiler.MergeEq
import cspom.compiler.RemoveUselessEq
import cspom.compiler.SplitEqVec
import cspom.compiler.ConstraintCompiler

object ConcretePatterns {

  def apply(params: ParameterManager): Seq[ConstraintCompiler] = {
    val concreteDef =
      Seq(
        XCSPPatterns(),
        DivToMul, AbsDiff, AbsDiffDomains, BoolEq,
        MulDomains, EqDomains, AbsDomains, NeqToEq,
        Bool2IntDomains, MulToSum, SumFactors, SumConstants, SumDomains, PseudoBool,
        MergeNotDisj,
        NegToCNF, Xor, ReifiedConj, ReifiedClause,
        NeqVec, SimplClause,
        SlidingSum, Regular, SetIn, ConcreteTypes,
        SumNe, AllDifferent, AllDiffConstant, CountEq //, ConstToVar
        )

    val concreteImp = Seq(
      AllDiff, SubsumedDiff, DiffGe, Square, UnaryClause) //, LexLeq2SAT)

    val improveModel = params.getOrElse("improveModel", true)

    if (improveModel) {
      StandardCompilers() ++ StandardCompilers.improve() ++ concreteDef ++ concreteImp
    } else {
      StandardCompilers() ++ concreteDef
    }

  }

}