package concrete.constraint.semantic;

import org.scalacheck.Gen
import org.scalatest.FlatSpec
import org.scalatest.Inspectors
import org.scalatest.Matchers
import org.scalatest.prop.PropertyChecks
import concrete.BooleanDomain
import concrete.IntDomain
import concrete.Variable
import concrete.constraint.Constraint
import concrete.constraint.ResiduesRemovals
import concrete.constraint.TupleEnumerator
import concrete.Problem
import concrete.constraint.ConstraintComparator

final class MulTest extends FlatSpec with Matchers with PropertyChecks with Inspectors {

  val dom = Gen.nonEmptyListOf(Gen.choose(-1000, 1000))

  "Mul" should "filter the same as enumerators" in {

    forAll(dom, dom, dom) { (x: Seq[Int], y: Seq[Int], z: Seq[Int]) =>
      val vx = new Variable("x", IntDomain.ofSeq(x: _*))
      val vy = new Variable("y", IntDomain.ofSeq(y: _*))
      val vz = new Variable("z", IntDomain.ofSeq(z: _*))

      ConstraintComparator.compare(
        Array(vx, vy, vz),
        new MulAC(vx, vy, vz),
        new Constraint(Array(vx, vy, vz)) with ResiduesRemovals with TupleEnumerator {
          def check(t: Array[Int]) = t(0) == t(1) * t(2);
        })
      //      
      //      val Revised(mod2, _, _) = c2.revise(mod)
      //      forAll(mod zip mod2) { case (m1, m2) => m1 should be theSameInstanceAs m2 }
      //      println(s"$domains $mod $mod2")
    }
  }

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
}
