package concrete.constraint.semantic

import concrete._
import concrete.constraint.AdviseCount
import concrete.filter.ACC
import org.scalatest.Inspectors
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


class InverseTest extends AnyFlatSpec with Matchers with Inspectors {
  "Inverse" should "filter" in {
    val x = Array.tabulate(4)(i => new Variable(s"x[$i]", IntDomain(0 to 3)))
    val y = Array.tabulate(4)(i => new Variable(s"y[$i]", IntDomain(0 to 3)))


    val prob = new Problem(x ++ y)

    val ac = new AdviseCount
    val xtoy = new Inverse(x, y, 0, 0).register(ac)
    val ytox = new Inverse(y, x, 0, 0).register(ac)

    prob.addConstraint(xtoy)
    prob.addConstraint(ytox)


    val state = prob.initState
      .assign(x(1), 3)
      .toState

//    println(state)

    val acc = new ACC(prob, new ParameterManager)

    val s2 = acc.reduceAfter(Seq((x(1), Assignment)), Seq(), state).toState

//    println(s2)

    s2.dom(x(1)) should contain theSameElementsAs Seq(3)
    s2.dom(y(3)) should contain theSameElementsAs Seq(1)
    forAll(List(0, 2, 3)) (i => s2.dom(x(i)) should contain theSameElementsAs (0 to 2) )
    forAll(0 to 2) { i: Int => s2.dom(y(i)) should contain theSameElementsAs Seq(0, 2, 3) }
  }
}
