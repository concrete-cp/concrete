package concrete.constraint.semantic

import concrete._
import concrete.constraint._
import org.scalacheck.Gen
import org.scalatest.{FlatSpec, Inspectors, Matchers}
import org.scalatest.prop.PropertyChecks

/**
  * @author vion
  */
class DivTest extends FlatSpec with Matchers with PropertyChecks {
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
          } shouldBe a [Contradiction]


        // mod.dom(z).view should contain theSameElementsAs Seq(zv)
  }

  private val dom = Gen.nonEmptyListOf(Gen.choose(-1000, 1000))

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
}