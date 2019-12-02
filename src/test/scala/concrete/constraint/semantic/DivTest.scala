package concrete.constraint.semantic

import concrete._
import concrete.constraint._
import concrete.util.Interval
import org.scalacheck.Gen
import org.scalatest.Inspectors
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
  * @author vion
  */
class DivTest extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks {
  "DivAC" should "comply with MiniZinc specifications" in {

    Inspectors.forAll(Seq((7, 4, 1), (-7, 4, -1), (7, -4, -1), (-7, -4, 1))) {
      case (xv, yv, zv) =>

        val x = new Variable("x", Singleton(xv))
        val y = new Variable("y", Singleton(yv))
        val z = new Variable("z", IntDomain.ofInterval(-1000, 1000))

        val problem = Problem(x, y, z)
        val constraint = new DivAC(x, y, z, skipIntervals = false)
        problem.addConstraint(constraint)
        constraint.register(new AdviseCount)

        val mod = problem.initState
          .andThen { state =>
            constraint.eventAll(state)
            constraint.revise(state)
          }
          .toState

        mod.dom(z).view should contain theSameElementsAs Seq(zv)
    }
  }

  it should "filter division by zero" in {
    val x = new Variable("x", Singleton(0))
    val y = new Variable("y", Singleton(0))
    val z = new Variable("z", Singleton(0))

    val problem = Problem(x, y, z)
    val constraint = new DivAC(x, y, z, skipIntervals = false)
    problem.addConstraint(constraint)
    constraint.register(new AdviseCount)

    problem.initState
      .andThen { state =>
        constraint.eventAll(state)
        constraint.revise(state)
      } shouldBe a[Contradiction]


    // mod.dom(z).view should contain theSameElementsAs Seq(zv)
  }


  private val dom = Gen.nonEmptyListOf(Gen.choose(-100, 100))

  it should "filter the same as enumerators" in {

    forAll(dom, dom, dom) { (x: Seq[Int], y: Seq[Int], z: Seq[Int]) =>
      val vx = new Variable("x", IntDomain.ofSeq(x: _*))
      val vy = new Variable("y", IntDomain.ofSeq(y: _*))
      val vz = new Variable("z", IntDomain.ofSeq(z: _*))

      val c = new DivAC(vx, vy, vz, skipIntervals = false)

      ConstraintComparator.compare(
        Array(vx, vy, vz),
        c,
        new Constraint(Array(vx, vy, vz)) with Residues with TupleEnumerator {
          def check(t: Array[Int]): Boolean = c.check(t)
        })

    }
  }

  it should "filter the same as enumerators, test case" in {

    val vx = new Variable("x", IntDomain.ofSeq(76, -4, 47, 23, -11, -58, -51, -51, 38, -83, 91, -61, 60))
    val vy = new Variable("y", IntDomain.ofSeq(9))
    val vz = new Variable("z", IntDomain.ofSeq(0))
    val c = new DivAC(vx, vy, vz, skipIntervals = false)

    ConstraintComparator.compare(
      Array(vx, vy, vz),
      c,
      new Constraint(Array(vx, vy, vz)) with Residues with TupleEnumerator {
        def check(t: Array[Int]): Boolean = c.check(t)
      })
  }

  "DivBC" should "comply with MiniZinc specifications" in {

    Inspectors.forAll(Seq((7, 4, 1), (-7, 4, -1), (7, -4, -1), (-7, -4, 1))) {
      case (xv, yv, zv) =>
        val x = new Variable("x", Singleton(xv))
        val y = new Variable("y", Singleton(yv))
        val z = new Variable("z", IntDomain.ofInterval(-1000, 1000))

        val problem = Problem(x, y, z)
        val constraint = new DivBC(x, y, z)
        constraint.register(new AdviseCount)
        problem.addConstraint(constraint)
        val state = problem.initState.toState
        constraint.eventAll(state)
        val mod = constraint.revise(state)

        mod.dom(z).view should contain theSameElementsAs Seq(zv)
    }
  }


  private val itvDom = for (lb <- Gen.choose(-100, 100); ub <- Gen.choose(lb, 100)) yield Interval(lb, ub)

  it should "shave the same as enumerator" in {
    ScalaCheckPropertyChecks.forAll(itvDom, itvDom, itvDom) { (x, y, z) =>
      val vx = new Variable("x", IntDomain.ofInterval(x))
      val vy = new Variable("y", IntDomain.ofInterval(y))
      val vz = new Variable("z", IntDomain.ofInterval(z))

      val c = new DivBC(vx, vy, vz)

      ConstraintComparator.compareSubset(
        Array(vx, vy, vz),
        c,
        new Constraint(vx, vy, vz) with TupleEnumerator with BoundResidues {
          def check(t: Array[Int]): Boolean = c.check(t)

          override def init(ps: ProblemState): Outcome = ps
        })
    }
  }
}