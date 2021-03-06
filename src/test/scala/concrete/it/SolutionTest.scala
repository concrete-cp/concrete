package concrete
package it

import org.scalatest.FlatSpec
import concrete.generator.ProblemGenerator
import concrete.generator.cspompatterns.ConcretePatterns
import concrete.runner.FZConcrete
import cspom.compiler.CSPOMCompiler

import scala.collection.{immutable, mutable}
import scala.util.Random

class SolutionTest extends FlatSpec {

  /*
 * 
 * deltaK = array3d(0..4, 0..3, 0..3, [1, 1, 1, 1, 1, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0]);
deltaSR = array3d(0..4, 0..3, 0..3, [1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]);
deltaX = array3d(0..4, 0..3, 0..3, [1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]);
deltaY = array3d(0..3, 0..3, 0..3, [1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0]);
 * 
 */

  val deltaK = Array(1, 1, 1, 1, 1, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0)
  val deltaSR = Array(1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  val deltaX = Array(1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  val deltaY = Array(1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0)

  val rand = new Random(0)

  "concrete" should "validate solution" in {

    val pm = new ParameterManager()
//      .updated("ac3c.key", classOf[concrete.heuristic.revision.Weight])
//      .updated("ac3c.queue", classOf[concrete.priorityqueues.ScalaBinomialHeap[Constraint]])
//      .updated("improveModel", false)

    //    val cspom = CSPOM { implicit problem =>
    //      val x = IntVariable(0 to 1) as "X"
    //      val y = IntVariable(0 to 1)
    //      val z = IntVariable(0 to 1)
    //      val t = IntVariable(0 to 1)
    //
    //      ctr(x !== y)
    //      ctr(x !== z)
    //      ctr(y !== z)
    //      ctr(x !== t)
    //    };

    val status = for {
      cspom0 <- FZConcrete.loadCSPOMURL(pm, classOf[SolutionTest].getResource("step1_aes-kb128_n5_obj16.fzn.xz"))
      cspom <- CSPOMCompiler.compile(cspom0, ConcretePatterns(pm))
      prob <- new ProblemGenerator(pm).generate(cspom)
    } yield {
      val (pb, vars) = prob


      val solver: MAC = MAC(pb, immutable.ArraySeq.unsafeWrapArray(pb.variables), pm).get

      var state = pb.initState.andThen(solver.init).andThen(solver.preprocess(solver.filter, _))

      val decisions = Seq.tabulate(80) { i =>
        val variable = cspom.variable(s"deltaK[${i + 1}]").map(vars).get
        state = state.assign(variable, deltaK(i)).toState
        variable
      } ++ Seq.tabulate(80) { i =>

        val variable = cspom.variable(s"deltaSR[${i + 1}]").map(vars).get
        state = state.assign(variable, deltaSR(i)).toState
        variable
      } ++ Seq.tabulate(80) { i =>

        val variable = cspom.variable(s"deltaX[${i + 1}]").map(vars).get
        state = state.assign(variable, deltaX(i)).toState
        variable
      } ++ Seq.tabulate(64) { i =>

        val variable = cspom.variable(s"deltaY[${i + 1}]").map(vars).get
        state = state.assign(variable, deltaY(i)).toState
        variable
      }


      assert(solver.oneRun(decisions.map((_, Assignment)), Stack(state), None)._1.isSat)

    }

    status.get

  }

}