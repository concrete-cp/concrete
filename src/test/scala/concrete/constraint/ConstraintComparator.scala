package concrete.constraint

import concrete._
import org.scalatest.{Inspectors, Matchers}

/**
  * @author vion
  */
object ConstraintComparator extends Matchers with Inspectors {

  def checkContradiction(c: Constraint): Unit = {
    val problem = new Problem(c.scope)
    problem.addConstraint(c)
    val ac = new AdviseCount
    c match {
      case c: Advisable => c.register(ac)
      case _ => ()
    }

    problem.initState.andThen { ps =>
      c.eventAll(ps, Assignment)
      c.revise(ps)
    } shouldBe a [Contradiction]


  }

  def compare(vars: Array[Variable], c1: Constraint, c2: Constraint): Unit = {
    val problem = new Problem(vars)
    problem.addConstraint(c1)
    problem.addConstraint(c2)
    val ac = new AdviseCount
    Seq(c1, c2)
      .collect {
        case c: Advisable => c
      }
      .foreach(_.register(ac))

    problem.initState.andThen { ps =>

      c1.eventAll(ps, Assignment)
      c2.eventAll(ps, Assignment)

      val r1 = c1.revise(ps)
      val r2 = c2.revise(ps)

      (r1, r2) match {
        case (_: Contradiction, _: Contradiction) =>
        case (s1: ProblemState, s2: ProblemState) =>
          forAll(s1.currentDomains.zip(s2.currentDomains).toSeq) { case (d1: Domain, d2: Domain) =>
            d1 should contain theSameElementsAs d2
          }
          assert(!s2.entailed.hasInactiveVar(c2) || s1.entailed.hasInactiveVar(c1))
        case (s1, s2) => fail(s"$s1 and $s2 were not the same")
      }

      ps
    }
  }

  def compareSubset(vars: Array[Variable], c1: Constraint, c2: Constraint): Unit = {
    val problem = new Problem(vars)
    problem.addConstraint(c1)
    problem.addConstraint(c2)
    val ac = new AdviseCount
    Seq(c1, c2)
      .collect {
        case c: Advisable => c
      }
      .foreach(_.register(ac))

    problem.initState.andThen { ps =>

      c1.eventAll(ps, Assignment)
      c2.eventAll(ps, Assignment)

      val r1 = c1.revise(ps)
      val r2 = c2.revise(ps)

      (r1, r2) match {
        case (_, _: Contradiction) =>
        case (s1: ProblemState, s2: ProblemState) =>
          forAll(s1.currentDomains.zip(s2.currentDomains).toSeq) { case (d1: Domain, d2: Domain) =>
            d1 should contain allElementsOf d2
          }
          assert(!s2.entailed.hasInactiveVar(c2) || s1.entailed.hasInactiveVar(c1))
        case (_: Contradiction, s2: ProblemState) => fail(s"$c1 triggered a contradiction although $c2 is feasible: $s2")
      }

      ps
    }
  }

}