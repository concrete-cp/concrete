package concrete.generator.cspompatterns

import scala.IndexedSeq
import cspom.util.GuavaRange
import cspom.util.IntervalsArithmetic
import cspom.util.IntervalsArithmetic.Arithmetics
import cspom.util.RangeSet
import cspom.compiler.VariableCompiler
import cspom.CSPOMConstraint
import cspom.variable.IntVariable
import cspom.variable.SimpleExpression

object AbsDiffDomains {
  val v = new VariableCompiler('absdiff, {
    case CSPOMConstraint(r: SimpleExpression[Int], _, Seq(i0: SimpleExpression[Int], i1: SimpleExpression[Int]), _) =>
      Map(
        r -> (r intersected IntVariable((i0 - i1).abs)
        i0 -> (i0 intersected IntVariable {
          val g1: RangeSet[Int] = i1 + r
          val g2: RangeSet[Int] = i1 - r

          g1 ++ g2
        }),
        i1 -> (i1 intersected IntVariable {
          val g1: RangeSet[Int] = IntervalsArithmetic(_ + _, i0, r)
          val g2: RangeSet[Int] = IntervalsArithmetic(_ - _, i0, r)
          g1 ++ g2
        }))
  })
}