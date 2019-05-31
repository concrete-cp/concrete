package concrete.constraint.semantic


import concrete.constraint._
import concrete._
import concrete.util.Interval
import org.scalacheck.Gen
import org.scalatest.{FlatSpec, Matchers}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

final class MulTest extends FlatSpec with Matchers with ScalaCheckPropertyChecks {

  private val dom = Gen.nonEmptyListOf(Gen.choose(-1000, 1000))

  "Mul" should "filter the same as enumerators" in {

    forAll(dom, dom, dom) { (x: Seq[Int], y: Seq[Int], z: Seq[Int]) =>
      val vx = new Variable("x", IntDomain.ofSeq(x: _*))
      val vy = new Variable("y", IntDomain.ofSeq(y: _*))
      val vz = new Variable("z", IntDomain.ofSeq(z: _*))

      ConstraintComparator.compare(
        Array(vx, vy, vz),
        new MulAC(vx, vy, vz),
        new Constraint(Array(vx, vy, vz)) with Residues with TupleEnumerator {
          def check(t: Array[Int]): Boolean = t(0) == t(1) * t(2)
        })
      //      
      //      val Revised(mod2, _, _) = c2.revise(mod)
      //      forAll(mod zip mod2) { case (m1, m2) => m1 should be theSameInstanceAs m2 }
      //      println(s"$domains $mod $mod2")
    }
  }

  private val itvDom = for (lb <- Gen.choose(-100, 100); ub <- Gen.choose(lb, 100)) yield Interval(lb, ub)

  it should "find allowed tuple" in {
    val r = new Variable("r", IntDomain.ofInterval(0, 3))
    val x = new Variable("x", IntDomain.ofInterval(0, 3))
    val y = new Variable("y", BooleanDomain())

    val pb = Problem(r, x, y)
    val c = new MulAC(r, x, y)
    pb.addConstraint(c)
    val state = pb.initState.toState

    val tuple = c.findSupport(c.scope.map(state.dom), 0, 2).get

    tuple(2) should be <= 1

  }

  "MulBC" should "filter the same as enumerators" in {

    forAll(itvDom, itvDom, itvDom) { (x: Interval, y: Interval, z: Interval) =>
      val vx = new Variable("x", IntDomain.ofInterval(x))
      val vy = new Variable("y", IntDomain.ofInterval(y))
      val vz = new Variable("z", IntDomain.ofInterval(z))

      ConstraintComparator.compareSubset(
        Array(vx, vy, vz),
        new MulBC(vx, vy, vz),
        new Constraint(Array(vx, vy, vz)) with BoundResidues with TupleEnumerator {
          def check(t: Array[Int]): Boolean = t(0) == t(1) * t(2)

          override def init(ps: ProblemState): Outcome = ps
        })
      //
      //      val Revised(mod2, _, _) = c2.revise(mod)
      //      forAll(mod zip mod2) { case (m1, m2) => m1 should be theSameInstanceAs m2 }
      //      println(s"$domains $mod $mod2")
    }
  }
}
