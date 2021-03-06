package concrete.constraint.semantic

import concrete._
import concrete.constraint.AdviseCount
import org.scalatest.{FlatSpec, Inspectors, Matchers}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

/**
  * @author vion
  */
class MinMaxTest extends FlatSpec with Matchers with Inspectors with ScalaCheckPropertyChecks {
  "Min" should "filter unique candidate" in {
    val r = new Variable("r", IntDomain.ofSeq(121))
    val a = Array(
      new Variable("a0", IntDomain.ofSeq(1000477)),
      new Variable("a1", IntDomain.ofInterval(100, 1000000)))

    val problem = Problem(r +: a: _*)
    val constraint = Min(r, a)
    problem.addConstraint(constraint)
    val state = problem.initState.toState
    constraint.register(new AdviseCount)
    constraint.eventAll(state, Assignment)
    val mod = constraint.revise(state)

    mod.dom(a(1)) shouldBe Singleton(121)

  }

  it should "filter given case" in {
    val r = new Variable("r", IntDomain.ofSeq(95))
    val a = Array(
      new Variable("a0", IntDomain.ofSeq(219)),
      new Variable("a1", IntDomain.ofSeq(131)),
      new Variable("a2", IntDomain.ofSeq(51, 63, 74, 112, 367)))

    val problem = Problem(r +: a: _*)
    val constraint = Min(r, a)
    problem.addConstraint(constraint)
    val state = problem.initState.toState
    constraint.register(new AdviseCount)
    constraint.eventAll(state, Assignment)
    val mod = constraint.revise(state)

    assert(!mod.isState)
  }

  it should "filter another given case" in {
    val r = new Variable("r", IntDomain.ofSeq(41, 52, 64, 86, 187))
    val a = Array(
      new Variable("a0", IntDomain.ofSeq(334)),
      new Variable("a1", IntDomain.ofSeq(22, 86, 87, 115, 187)),
      new Variable("a2", IntDomain.ofSeq(22, 86, 87, 115, 252)))

    val problem = Problem(r +: a: _*)
    val constraint = Min(r, a)
    problem.addConstraint(constraint)
    val state = problem.initState.toState
    constraint.register(new AdviseCount)
    constraint.eventAll(state, Assignment)
    val mod = constraint.revise(state)

    forAll(r +: a) { v =>
      assert(mod.dom(v).head >= 86)
    }


  }

  it should "filter result" in {
    val r = new Variable("r", IntDomain.ofInterval(121, 10000))
    val a = Array(
      new Variable("a0", IntDomain.ofSeq(1000477)),
      new Variable("a1", IntDomain.ofInterval(125, 1000000)))

    val problem = Problem(r +: a: _*)
    val constraint = Min(r, a)
    problem.addConstraint(constraint)
    val state = problem.initState.toState
    constraint.register(new AdviseCount)
    constraint.eventAll(state, Assignment)
    val mod = constraint.revise(state)

    withClue(constraint.toString(mod.toState)) {
      mod.dom(r).head shouldBe 125
    }
  }

  it should "filter all candidates" in {
    val r = new Variable("r", IntDomain.ofSeq(121))
    val a = Array(
      new Variable("a0", IntDomain.ofSeq(50, 1000477)),
      new Variable("a1", IntDomain.ofInterval(100, 1000000)))

    val problem = Problem(r +: a: _*)
    val constraint = Min(r, a)
    problem.addConstraint(constraint)
    val state = problem.initState.toState
    constraint.register(new AdviseCount)
    constraint.eventAll(state, Assignment)
    val mod = constraint.revise(state)

    forAll(a)(v => mod.dom(v).head should be >= 121)
  }

  it should "detect inconsistency when result does not intersect with variables" in {
    val r = new Variable("r", IntDomain.ofSeq(0))
    val a = Array(
      new Variable("a0", IntDomain.ofSeq(1)),
      new Variable("a1", IntDomain.ofSeq(-3, -2)))

    val problem = Problem(r +: a: _*)
    val constraint = Min(r, a)
    problem.addConstraint(constraint)
    val state = problem.initState.toState
    constraint.register(new AdviseCount)
    constraint.eventAll(state, Assignment)
    assert(!constraint.revise(state).isState)
  }

  it should "not promote interval" in {
    val r = new Variable("r", IntDomain.ofSeq(1, 2, 3, 5, 6, 7, 8, 9, 10, 11))
    val a = Array(
      new Variable("a0", IntDomain.ofSeq(1, 2, 3, 5, 6, 7, 8, 9, 10, 11)),
      new Variable("a1", IntDomain.ofSeq(1, 2, 3, 5, 6, 7, 8)))

    val problem = Problem(r +: a: _*)
    val constraint = Min(r, a)
    problem.addConstraint(constraint)
    val state = problem.initState.toState
    constraint.register(new AdviseCount)
    constraint.eventAll(state, Assignment)
    val mod = constraint.revise(state)
    mod.dom(r).view should contain theSameElementsAs mod.dom(a(1)).view
    mod.dom(a(0)) shouldBe state.dom(a(0))

    mod.toState(constraint) shouldBe a
  }

  it should "handle variables with no intersection with result" in {
    val vx = new Variable("x", IntDomain.ofSeq(0, 2))
    val vy = Array(
      new Variable(s"y[0]", IntDomain.ofSeq(-1, 1)),
      new Variable(s"y[1]", IntDomain.ofSeq(0, 1)))

    val constraint = Min(vx, vy)
    val problem = Problem(vx +: vy: _*)
    problem.addConstraint(constraint)
    val state = problem.initState.toState
    constraint.register(new AdviseCount)
    constraint.eventAll(state, Assignment)
    val mod = constraint.revise(state)
    mod.dom(vx).view should contain theSameElementsAs Seq(0)
    mod.dom(vy(0)).view should contain theSameElementsAs Seq(1)
    mod.dom(vy(1)).view should contain theSameElementsAs Seq(0)
  }

  it should "handle test case" in {
    val vx = new Variable("x", IntDomain.ofSeq(0, 2))
    val vy = Array(
      new Variable(s"y[0]", IntDomain.ofSeq(0, 2)),
      new Variable(s"y[1]", IntDomain.ofSeq(1)))

    val constraint = Min(vx, vy)
    val problem = Problem(vx +: vy: _*)
    problem.addConstraint(constraint)
    val state = problem.initState.toState
    constraint.register(new AdviseCount)
    constraint.eventAll(state, Assignment)
    val mod = constraint.revise(state)
    mod.dom(vx).view should contain theSameElementsAs Seq(0)
    mod.dom(vy(0)).view should contain theSameElementsAs Seq(0)
    mod.dom(vy(1)).view should contain theSameElementsAs Seq(1)
  }

  it should "not have a contradiction" in {
    val vx = new Variable("x", IntDomain.ofInterval(1, 7))
    val vy = Array(
      new Variable(s"y[0]", IntDomain.ofInterval(1, 11)),
      new Variable(s"y[1]", IntDomain.ofInterval(1, 11)))

    val constraint = Min(vx, vy)
    val problem = Problem(vx +: vy: _*)
    problem.addConstraint(constraint)
    val state = problem.initState.toState
    constraint.register(new AdviseCount)
    constraint.eventAll(state, Assignment)
    val mod = constraint.revise(state)

    mod shouldBe state


  }

  //  it should "filter values not in result domain and not subsumed by other min" in {
  //    val vx = new Variable("x", IntDomain.ofSeq(0, 2))
  //    val vy = Array(
  //      new Variable(s"y[0]", IntDomain.ofSeq(1, 2)),
  //      new Variable(s"y[1]", IntDomain.ofSeq(0, 1, 2)))
  //
  //    val constraint = Min(vx, vy)
  //    val problem = Problem(vx +: vy: _*)
  //    problem.addConstraint(constraint)
  //    val state = problem.initState.toState
  //
  //    val mod = constraint.revise(state)
  //    mod.dom(vx) should contain theSameElementsAs Seq(0, 2)
  //    mod.dom(vy(0)) should contain theSameElementsAs Seq(1, 2)
  //    mod.dom(vy(1)) should contain theSameElementsAs Seq(0, 2)
  //  }

  //  val dom = Gen.nonEmptyListOf(Gen.choose(-1000, 1000))
  //
  //  it should "filter the same as enumerator" in {
  //
  //    forAll(dom, Gen.listOfN(2, dom)) { (x, y) =>
  //      whenever(y.size == 2) {
  //        val vx = new Variable("x", IntDomain.ofSeq(x: _*))
  //        val vy = (y.zipWithIndex).map { case (dy, i) => new Variable(s"y[$i]", IntDomain.ofSeq(dy: _*)) }
  //
  //        println(vx.initDomain + " = min(" + vy.map(_.initDomain).mkString(", ") + ")")
  //
  //        ConstraintComparator.compare(
  //          vx :: vy,
  //          Min(vx, vy.toArray),
  //          new Constraint((vx :: vy).toArray) with Residues with TupleEnumerator {
  //            def check(t: Array[Int]) = t(0) == t.tail.min
  //          })
  //      }
  //
  //    }
  //
  //  }
  //
  //  it should "filter the same as enumerator, test case" in {
  //    val vx = new Variable("x", IntDomain.ofSeq(0, 3))
  //    val vy = Array(
  //      new Variable(s"y[0]", IntDomain.ofSeq(1, 3)),
  //      new Variable(s"y[1]", IntDomain.ofSeq(0, 2, 3)))
  //
  //    val constraint = Min(vx, vy)
  //    val problem = Problem(vx +: vy: _*)
  //    problem.addConstraint(constraint)
  //    val state = problem.initState.toState
  //
  //    val mod = constraint.revise(state)
  //    mod.dom(vx) should contain theSameElementsAs Seq(0, 3)
  //    mod.dom(vy(0)) should contain theSameElementsAs Seq(1, 3)
  //    mod.dom(vy(1)) should contain theSameElementsAs Seq(0, 3)
  //    assert(!mod.isEntailed(constraint))
  //
  //    ConstraintComparator.compare(
  //      vx :: vy.toList,
  //      Min(vx, vy.toArray),
  //      new Constraint(vx +: vy) with Residues with TupleEnumerator {
  //        def check(t: Array[Int]) = t(0) == t.tail.min
  //      })
  //  }

  "Max" should "filter unique candidate" in {
    val r = new Variable("r", IntDomain.ofSeq(200000))
    val a = Array(
      new Variable("a0", IntDomain.ofSeq(100477)),
      new Variable("a1", IntDomain.ofInterval(100, 1000000)))

    val problem = Problem(r +: a: _*)
    val constraint = Max(r, a)
    problem.addConstraint(constraint)
    val state = problem.initState.toState
    constraint.register(new AdviseCount)
    constraint.eventAll(state, Assignment)

    val mod = constraint.revise(state)

    mod.dom(a(1)) shouldBe Singleton(200000)

  }

  it should "filter result" in {
    val r = new Variable("r", IntDomain.ofInterval(121, 10000))
    val a = Array(
      new Variable("a0", IntDomain.ofSeq(1477)),
      new Variable("a1", IntDomain.ofInterval(125, 1000)))

    val problem = Problem(r +: a: _*)
    val constraint = Max(r, a)
    problem.addConstraint(constraint)
    val state = problem.initState.toState
    constraint.register(new AdviseCount)
    constraint.eventAll(state, Assignment)


    val mod = constraint.revise(state)

    mod.dom(r) shouldBe Singleton(1477)

  }

  it should "filter all candidates" in {
    val r = new Variable("r", IntDomain.ofSeq(121))
    val a = Array(
      new Variable("a0", IntDomain.ofSeq(50, 1000477)),
      new Variable("a1", IntDomain.ofInterval(100, 1000000)))

    val problem = Problem(r +: a: _*)
    val constraint = Max(r, a)
    problem.addConstraint(constraint)
    val state = problem.initState.toState
    constraint.register(new AdviseCount)
    constraint.eventAll(state, Assignment)
    val mod = constraint.revise(state)

    forAll(a)(v => mod.dom(v).last should be <= 121)
  }
}