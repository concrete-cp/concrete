package concrete.constraint.extension

import scala.collection.SortedSet
import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatest.prop.PropertyChecks
import concrete.IntDomain
import concrete.Variable
import org.scalatest.Inspectors

class MDDRelationTest extends FlatSpec with Matchers with PropertyChecks {

  def relation(arity: Int) = Gen.nonEmptyListOf(Gen.containerOfN[Array, Int](arity, Arbitrary.arbitrary[Int]))

  def domains = Gen.containerOfN[Array, SortedSet[Int]](5, Arbitrary.arbitrary[SortedSet[Int]].suchThat(_.nonEmpty))

  def pickValue(scope: Array[Variable]) = {
    for (
      p <- Gen.oneOf(scope.indices);
      i <- Gen.oneOf(scope(p).dom.indices.toSeq)
    ) yield {
      (p, i)
    }
  }

  "MDDs" should "work" in {

    forAll(domains) { doms =>
      val scope = doms.map(d => new Variable(null, IntDomain(d)))
      require(scope.size == 5)
      println(scope.mkString(" "))
      forAll(relation(5)) { relation =>
        val mdd = MDDRelation(relation)
        forAll(pickValue(scope)) {
          case (p, i) =>
            mdd.findSupport(scope, p, i, new Array(scope.size)) match {
              case Some(t) =>
                t(p) shouldBe i
                mdd should contain(t)
              case None =>
                Inspectors.forAll(mdd) { t: Array[Int] => t(p) should not be (i) }
            }
        }
      }

    }

  }

}