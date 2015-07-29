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

/**
 * @author vion
 */
class DivTest extends FlatSpec with Matchers with Inspectors {
  "DivAC" should "comply with MiniZinc specifications" in {

    forAll(Seq((7, 4, 1), (-7, 4, -1), (7, -4, -1), (-7, -4, 1))) {
      case (xv, yv, zv) =>

        val x = new Variable("x", Singleton(xv))
        val y = new Variable("y", Singleton(yv))
        val z = new Variable("z", IntDomain.ofInterval(-1000, 1000))

        val problem = Problem(x, y, z)
        val constraint = new DivAC(x, y, z)
        problem.addConstraint(constraint)
        val state = problem.initState.toState
        val advisor = new AdviseCount
        constraint.register(advisor)
        constraint.adviseAll(state)
        val mod = constraint.revise(state)

        mod.dom(z) should contain theSameElementsAs Seq(zv)
    }
  }

  // TODO
  //  "DivBC" should "comply with MiniZinc specifications" in {
  //
  //    forAll(Seq((7, 4, 1), (-7, 4, -1), (7, -4, -1), (-7, -4, 1))) {
  //      case (xv, yv, zv) =>
  //        val x = new Variable("x", Singleton(xv))
  //        val y = new Variable("y", Singleton(yv))
  //        val z = new Variable("z", IntDomain.ofInterval(-1000, 1000))
  //
  //        val problem = Problem(x, y, z)
  //        val constraint = new DivBC(x, y, z)
  //        problem.addConstraint(constraint)
  //        val state = problem.initState.toState
  //        constraint.adviseAll(state)
  //        val mod = constraint.revise(state)
  //
  //        mod.dom(z) should contain theSameElementsAs Seq(zv)
  //    }
  //  }
}