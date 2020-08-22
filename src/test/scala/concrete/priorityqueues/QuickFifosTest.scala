package concrete.priorityqueues

import bitvectors.BitVector
import concrete.constraint.Constraint

import scala.util.Random
import concrete._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class QuickFifosTest extends AnyFlatSpec with Matchers {

  class TestConstraint(val eval: Int, val variable: Variable) extends Constraint(variable) {
    def init(ps: ProblemState): ProblemState = ps
    def advise(ps: ProblemState, event: Event, p: Int): Int = eval
    def revise(ps: ProblemState, mod: BitVector): Outcome = ps
    def simpleEvaluation = 1
    def check(t: Array[Int]) = true
  }

  "QuickFifos" should "enqueue and dequeue" in {
    val v = new Variable("foo", IntDomain(0 to 1))
    val q = new QuickFifos[Constraint]()
    val r = new Random()
    assert(q.isEmpty)
    for (i <- 0 until 10) {
      val c = new TestConstraint(r.nextInt(1000000), v)
      q.offer(c, c.eval)
    }

    for (i <- 0 until 10) {
      assert(!q.isEmpty)
      q.poll()
    }
    assert(q.isEmpty)

    for (i <- 0 until 10000) {
      val c = new TestConstraint(r.nextInt(1000000), v)
      q.offer(c, c.eval)
    }
    assert(!q.isEmpty)
    q.clear()
    assert(q.isEmpty)
  }
}