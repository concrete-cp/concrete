package concrete.priorityqueues

import concrete.constraint.Constraint
import scala.util.Random
import org.scalatest.Matchers
import org.scalatest.FlatSpec
import concrete.Domain
import concrete.ProblemState
import concrete.Variable
import concrete.IntDomain

class QuickFifosTest extends FlatSpec with Matchers {

  class TestConstraint(val eval: Int, val variable: Variable) extends Constraint(variable) {
    def init(ps: ProblemState) = ps
    def advise(ps: ProblemState, p: Int) = eval
    def revise(ps: ProblemState) = ps
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
      val c = q.poll()
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