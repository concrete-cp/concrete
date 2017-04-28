package concrete
package constraint
package semantic

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatest.prop.PropertyChecks
import org.scalacheck.Gen

class MemberTest extends FlatSpec with Matchers with PropertyChecks {
  "Member constraint" should "filter" in {
    val x = new Variable("x", IntDomain(0 to 1))
    val y = new Variable("y", IntDomain(1 to 10))
    val z = new Variable("z", IntDomain(2 to 10))

    val problem = Problem(x, y, z)
    val constraint = new Member(x, Array(y, z))
    problem.addConstraint(constraint)
    val ps = problem.initState.toState

    val mod = constraint.revise(ps).toState
    mod.dom(x).view should contain theSameElementsAs Seq(1)
    mod.dom(y).view should contain theSameElementsAs Seq(1)
    mod.dom(z).view should contain theSameElementsAs (2 to 10)
  }

  val dom = Gen.nonEmptyListOf(Gen.choose(-10, 10))

  it should "filter the same as enumerator" in {

    forAll(dom, dom, dom) { (x, y, z) =>

      val vx = new Variable("x", IntDomain.ofSeq(x: _*))
      val vy = new Variable(s"y", IntDomain.ofSeq(y: _*))
      val vz = new Variable(s"z", IntDomain.ofSeq(z: _*))

     

      ConstraintComparator.compare(
        Array(vx, vy, vz),
        new Member(vx, Array(vy, vz)),
        new Constraint(Array(vx, vy, vz)) with Residues with TupleEnumerator {
          def check(t: Array[Int]) = t.tail.contains(t(0))
        })
    }

  }
}