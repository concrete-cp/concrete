package concrete.generator.cspompatterns

import scala.IndexedSeq
import cspom.compiler.IntDomainGenerator
import cspom.util.GuavaRange
import cspom.util.IntervalsArithmetic
import cspom.util.IntervalsArithmetic.Arithmetics
import cspom.util.RangeSet

object AbsDomains extends IntDomainGenerator('abs, { _ =>
  IndexedSeq(
    {
      case Seq(i0, i1) =>
        IntervalsArithmetic((_.abs), IntervalsArithmetic((_ - _), i0, i1))
    },
    {
      case Seq(r, i1) =>
        val g1: RangeSet[Int] = IntervalsArithmetic(_ + _, i1, r)
        val g2: RangeSet[Int] = IntervalsArithmetic(_ - _, i1, r)

        g1 ++ g2
    },
    {
      case Seq(r, i0) =>
        val g1: RangeSet[Int] = IntervalsArithmetic(_ + _, i0, r)
        val g2: RangeSet[Int] = IntervalsArithmetic(_ - _, i0, r)
        g1 ++ g2
    })
})