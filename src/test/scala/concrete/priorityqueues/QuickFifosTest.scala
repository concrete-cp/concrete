package concrete.priorityqueues

import concrete.constraint.Constraint
import scala.util.Random
import org.scalatest.Matchers
import org.scalatest.FlatSpec

class QuickFifosTest extends FlatSpec with Matchers {

  class TestConstraint(val eval: Int) extends Constraint {
    def advise(p: Int) = eval
    def revise() = Nil
    def simpleEvaluation = 1
    def checkValues(t: Array[Int]) = true
  }

  "QuickFifos" should "enqueue and dequeue" in {

    val q = new QuickFifos[Constraint]()
    val r = new Random()
    assert(q.isEmpty)
    for (i <- 0 until 10) {
      val c = new TestConstraint(r.nextInt(1000000))
      q.offer(c, c.eval)
    }

    for (i <- 0 until 10) {
      assert(!q.isEmpty)
      val c = q.poll()
    }
    assert(q.isEmpty)

    for (i <- 0 until 10000) {
      val c = new TestConstraint(r.nextInt(1000000))
      q.offer(c, c.eval)
    }
    assert(!q.isEmpty)
    q.clear()
    assert(q.isEmpty)
  }
}