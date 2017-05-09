package concrete.generator.cspompatterns

import cspom.extension.Relation
import cspom.util.{ContiguousIntRangeSet, IntInterval, RangeSet}
import cspom.variable.IntVariable
import org.scalatest.{FlatSpec, Inspectors, Matchers}

class KnapsackTest extends FlatSpec with Matchers with Inspectors {

  "knapsack generator" should "generate correct mdd" in {
    val profits = IndexedSeq(5, 10)
    val weights = IndexedSeq(10, 15)

    val domains = IndexedSeq.fill(2)(IntVariable(0 to 3))
    val W = IntVariable(0 to 50)
    val P = IntVariable(0 to 30)

    val constraints = Knapsack.mdd(weights, profits, domains, W, P) //.reduce()

    constraints match {
      case Seq(c) =>
        val mdd: Iterable[Seq[Int]] = c.getParam[Relation[Int]]("relation").get

        forAll(mdd) { t: Seq[Int] =>
          val quantities :+ w :+ p  = t
          (quantities, weights).zipped.map(_ * _).sum shouldBe w
          (quantities, profits).zipped.map(_ * _).sum shouldBe p
        }
    }


  }

  it should "handle large arities" in {
    val weights = IndexedSeq(40, 91, 10, 30, 160, 20, 3, 12, 3, 18, 9, 25, 1, 1, 10, 280, 10, 8, 1, 1, 49, 8, 21, 6, 1, 5, 10, 8, 2, 1, 0, 10, 42, 6, 4, 8, 0, 10, 1, 40, 86, 11, 120, 8, 3, 32, 28, 13, 2, 4)
      .take(20)
    val profits = IndexedSeq(560, 1125, 300, 620, 2100, 431, 68, 328, 47, 122, 322, 196, 41, 25, 425, 4260, 416, 115, 82, 22, 631, 132, 420, 86, 42, 103, 215, 81, 91, 26, 49, 420, 316, 72, 71, 49, 108, 116, 90, 738, 1811, 430, 3060, 215, 58, 296, 620, 418, 47, 81)
      .take(20)

    val size = weights.size
    profits should have size size

    val domains = IndexedSeq.fill(weights.size)(RangeSet(IntInterval(0, 1)))

    val W = RangeSet(IntInterval(0, 800))

    val P = RangeSet(IntInterval.singleton(10000)) ///16537)

    val mdd1 = Knapsack.mddSum(weights, domains, W)
    val mdd2 = Knapsack.mddSum(profits, domains, P)

    //println(mdd1)
    //mdd1.foreach(println)
    //println(mdd2)
    //mdd2.foreach(println)

    val mdd11 = mdd1.insertDim(weights.size, new ContiguousIntRangeSet(P).toSeq)

    //println(mdd11)
    //mdd11.foreach(println)

    val mdd22 = mdd2.insertDim(weights.size + 1, new ContiguousIntRangeSet(W).toSeq)

    //println(mdd22)
    //mdd22.foreach(println)

    mdd22.intersect(mdd11)
    //println(mddi)

    //println(mddi.reduce)
    //mddi.foreach(println)
    //println(mdd11.intersect(mdd22))

    //    println(mdd1.reduce())
    //    println(mdd2.reduce())
  }
}