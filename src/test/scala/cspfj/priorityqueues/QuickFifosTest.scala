package cspfj.priorityqueues

import org.junit.Test
import cspfj.constraint.Constraint
import scala.util.Random
import org.junit.Assert._

class QuickFifosTest {

  @Test
  def test() {
    val q = new QuickFifos()
    val r = new Random()
    assertTrue(q.isEmpty)
    for (i <- 0 until 10) {
      val c = new Constraint(Array()) {
        val eval = r.nextInt(1000000)
        def advise(p: Int) = eval
        def revise() = false
        def simpleEvaluation = 1
        def checkValues(t: Array[Int]) = true
      }
      q.offer(c, c.eval)
    }

    for (i <- 0 until 10) {
      assertFalse(q.isEmpty)
      val c = q.poll()
      println(c.advise(0))
    }
    assertTrue(q.isEmpty)

    for (i <- 0 until 10000) {
      val c = new Constraint(Array()) {
        val getEvaluation = r.nextInt(1000000)
        def revise() = false
        def simpleEvaluation = 1
        def checkValues(t: Array[Int]) = true
      }
      q.add(c)
    }
    assertFalse(q.isEmpty)
    q.clear()
    assertTrue(q.isEmpty)
  }
}