package concrete.constraint.semantic

import concrete.CSPOMDriver._
import cspom.CSPOM
import cspom.CSPOM._
import cspom.variable.CSPOMConstant
import org.scalatest.Matchers
import org.scalatest.FlatSpec
import concrete.Solver
import cspom.variable.IntVariable
import org.scalatest.Inspectors
import concrete.IntDomain
import concrete.Variable
import concrete.Singleton
import concrete.Variable
import concrete.Problem
import concrete.constraint.AdviseCount
import concrete.constraint.BoundResidues
import org.scalacheck.Gen
import concrete.constraint.Constraint
import concrete.constraint.TupleEnumerator
import org.scalatest.prop.PropertyChecks
import concrete.ProblemState

/**
 * @author vion
 */
class ModTest extends FlatSpec with Matchers with Inspectors with PropertyChecks {
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
        constraint.adviseAll(state)
        val mod = constraint.revise(state)

        mod.dom(z) should contain theSameElementsAs Seq(zv)
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
        val state = problem.initState.toState
        constraint.adviseAll(state)
        val mod = constraint.revise(state)

        mod.dom(z) should contain theSameElementsAs Seq(zv)
    }
  }

//  val dom = Gen.nonEmptyListOf(Gen.choose(-1000, 1000))
//  val ydom = Gen.nonEmptyListOf(Gen.choose(1, 1000))
//
//  it should "filter the same as enumerator" in {
//    {
//      val vx = new Variable("x", IntDomain.ofSeq(158))
//      val vy = new Variable("y", IntDomain.ofSeq(46, 94))
//      val vz = new Variable("z", IntDomain.ofSeq(0))
//
//      ConstraintComparator.compare(
//        List(vx, vy, vz),
//        new ModBC(vx, vy, vz),
//        new Constraint(vx, vy, vz) with BoundResidues with TupleEnumerator {
//          def check(t: Array[Int]) = t(0) % t(1) == t(2);
//          def advise(ps: ProblemState, pos: Int) = getEvaluation(ps)
//        })
//    }
//    forAll(dom, ydom, dom) { (x: Seq[Int], y: Seq[Int], z: Seq[Int]) =>
//      val vx = new Variable("x", IntDomain.ofSeq(x: _*))
//      val vy = new Variable("y", IntDomain.ofSeq(y: _*))
//      val vz = new Variable("z", IntDomain.ofSeq(z: _*))
//
//      println(x, y, z)
//
//      ConstraintComparator.compare(
//        List(vx, vy, vz),
//        new ModBC(vx, vy, vz),
//        new Constraint(vx, vy, vz) with BoundResidues with TupleEnumerator {
//          def check(t: Array[Int]) = t(0) % t(1) == t(2);
//          def advise(ps: ProblemState, pos: Int) = getEvaluation(ps)
//        })
//    }
//
//  }
}