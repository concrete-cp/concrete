package concrete.generator.cspompatterns
import cspom.CSPOM
import CSPOM._
import org.scalatest.Matchers
import cspom.variable.IntVariable
import org.scalatest.FlatSpec
import cspom.CSPOMConstraint
import concrete.CSPOMSolver
import concrete.Solver
import cspom.variable.CSPOMSeq
import cspom.StatisticsManager

class GlobalCardinalityClosedTest extends FlatSpec with Matchers {

//  "GlobalCardinalityClosed" should "be semantically correct" in {
//    var vars: CSPOMSeq[Int] = null
//    val cspom = CSPOM { implicit problem =>
//      vars = Seq.fill(4)(IntVariable(0 to 20)) as "vars"
//
//      ctr(CSPOMConstraint('global_cardinality_closed)(vars, Seq(0, 9, 11, 13, 15, 17, 19), Seq(2, 2, 2, 2, 2, 2, 2)))
//
//    }
//
//    val sm = new StatisticsManager
//    val solver = Solver(cspom).get
//    sm.register("solver", solver.solver)
//    // println(cspom)
//    solver.toIterable.foreach(println)
//    println(sm.toString())
//  }

}