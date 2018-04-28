package concrete
package constraint
package semantic

import concrete.util.Interval
import org.scalacheck.Gen
import org.scalatest.FlatSpec
import org.scalatest.Inspectors
import org.scalatest.Matchers
import org.scalatest.prop.PropertyChecks

/**
  * @author vion
  */
class ModTest extends FlatSpec with Matchers with Inspectors {
  "ModAC" should "comply with MiniZinc specifications" in {

    forAll(Seq((7, 4, 3), (-7, 4, -3), (7, -4, 3), (-7, -4, -3))) {
      case (xv, yv, zv) =>

        val x = new Variable("x", Singleton(xv))
        val y = new Variable("y", Singleton(yv))
        val z = new Variable("z", IntDomain.ofInterval(-1000, 1000))

        val problem = Problem(x, y, z)
        val constraint = new ModAC(x, y, z)
        problem.addConstraint(constraint)
        val state = problem.initState.toState
        val advisor = new AdviseCount
        constraint.register(advisor)
        constraint.eventAll(state)
        val mod = constraint.revise(state)

        mod.dom(z).view should contain theSameElementsAs Seq(zv)
    }
  }

  "ModBC" should "comply with MiniZinc specifications" in {

    forAll(Seq((7, 4, 3), (-7, 4, -3), (7, -4, 3), (-7, -4, -3))) {
      case (xv, yv, zv) =>
        val x = new Variable("x", Singleton(xv))
        val y = new Variable("y", Singleton(yv))
        val z = new Variable("z", IntDomain.ofInterval(-1000, 1000))

        val problem = Problem(x, y, z)
        val constraint = new ModBC(x, y, z)
        problem.addConstraint(constraint)
        constraint.register(new AdviseCount)
        val state = problem.initState.toState
        constraint.eventAll(state)
        val mod = constraint.revise(state)

        mod.dom(z).view should contain theSameElementsAs Seq(zv)
    }
  }

  private val itvDom = for (lb <- Gen.choose(-100, 100); ub <- Gen.choose(lb, 100)) yield Interval(lb, ub)

  it should "filter less than an enumerator" in {

    {
      val x = Interval(69, 74)
      val y= Interval(54, 75)
      val z = Interval(53, 61)
      val vx = new Variable("x", IntDomain.ofInterval(x))
      val vy = new Variable("y", IntDomain.ofInterval(y))
      val vz = new Variable("z", IntDomain.ofInterval(z))

      val c = new ModBC(vx, vy, vz)

      ConstraintComparator.compareSubset(
        Array(vx, vy, vz),
        c,
        new Constraint(vx, vy, vz) with TupleEnumerator with BoundResidues {
          def check(t: Array[Int]): Boolean = c.check(t)

          override def init(ps: ProblemState): Outcome = ps
        })
    }

    PropertyChecks.forAll(itvDom, itvDom, itvDom) { (x: Interval, y: Interval, z: Interval) =>
      val vx = new Variable("x", IntDomain.ofInterval(x))
      val vy = new Variable("y", IntDomain.ofInterval(y))
      val vz = new Variable("z", IntDomain.ofInterval(z))

      val c = new ModBC(vx, vy, vz)

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