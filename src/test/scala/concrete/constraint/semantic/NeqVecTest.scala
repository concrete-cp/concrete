package concrete.constraint.semantic

import concrete._
import concrete.constraint.AdviseCount
import org.scalacheck.Gen
import org.scalatest.{FlatSpec, Inspectors, Matchers}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import scala.collection.mutable

final class NeqVecTest extends FlatSpec with Matchers with ScalaCheckPropertyChecks with Inspectors {

  def project(relation: Seq[Seq[Int]]): IndexedSeq[Set[Int]] = {
    val projection = IndexedSeq.fill(relation.head.size)(new mutable.HashSet[Int])

    for (tuple <- relation; (value, index) <- tuple.zipWithIndex) {
      projection(index) += value
    }

    projection.map(_.toSet)
  }

  def matrix2(d2: Int): Gen[Seq[Seq[Int]]] = Gen.chooseNum(1, 3).flatMap { d1 =>
    Gen.listOfN(d1 * d2, Gen.chooseNum(-1, 1)).map { squareList =>
      squareList.grouped(d2).toSeq
    }
  }

  "NeqVec" should "filter correctly" in {

    forAll(matrix2(5), matrix2(5)) { (r0: Seq[Seq[Int]], r1: Seq[Seq[Int]]) =>
      whenever(r0.nonEmpty && r1.nonEmpty && r0.forall(_.size == 5) && r1.forall(_.size == 5)) {
        val dx = project(r0)
        val x = Array.tabulate(dx.size)(i => new Variable(s"x$i", IntDomain.ofSeq(dx(i).toSeq: _*)))

        val dy = project(r1)
        val y = Array.tabulate(dy.size)(i => new Variable(s"y$i", IntDomain.ofSeq(dy(i).toSeq: _*)))

        val c = new NeqVec(x, y)

        val pb = new Problem(x ++ y)
        pb.addConstraint(c)
        c.register(new AdviseCount)

        val ps = pb.initState
        val revised = ps.andThen { ps =>
          c.eventAll(ps)
          c.revise(ps)
        }

        if (dx.forall(_.size == 1)) {
          if (dy.forall(_.size == 1)) {
            revised.isState shouldBe (dx, dy).zipped.exists((x, y) => x.head != y.head)
          } else {
            forAtLeast(1, dx zip y) { case (d, v) =>
              forAtLeast(1, revised.dom(v).view)(_ != d.head)
            }
          }
        } else if (dy.forall(_.size == 1)) {
          forAtLeast(1, dy zip x) { case (d, v) =>
            forAtLeast(1, revised.dom(v).view)(_ != d.head)
          }
        } else {
          revised.toState.domains shouldBe ps.toState.domains
        }
      }

    }

  }

  it should "detect contradiction" in {
    val x = Array.tabulate(5)(i => new Variable(s"x$i", IntDomain.ofSeq(0)))
    val y = Array.tabulate(5)(i => new Variable(s"y$i", IntDomain.ofSeq(0)))

    val c = new NeqVec(x, y)

    val pb = new Problem(x ++ y)
    pb.addConstraint(c)
    c.register(new AdviseCount)
    val revised = pb.initState.andThen { ps =>
      c.eventAll(ps)
      c.revise(ps)
    }

    assert(!revised.isState)
  }

}