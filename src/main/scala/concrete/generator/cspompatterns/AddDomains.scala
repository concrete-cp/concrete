package concrete.generator.cspompatterns

import scala.IndexedSeq
import cspom.compiler.IntDomainGenerator
import cspom.util.GuavaRange
import cspom.util.IntervalsArithmetic
import cspom.util.IntervalsArithmetic.Arithmetics
import cspom.util.RangeSet

object AddDomains extends IntDomainGenerator('add, { _ =>
  IndexedSeq(
    {
      case Seq(i0, i1) =>
        IntervalsArithmetic(_ + _, i0, i1)
    },
    {
      case Seq(r, i1) => IntervalsArithmetic(_ - _, r, i1)
    },
    {
      case Seq(r, i0) => IntervalsArithmetic(_ - _, r, i0)
    })
})