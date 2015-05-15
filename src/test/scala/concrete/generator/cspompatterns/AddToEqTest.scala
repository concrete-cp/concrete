package concrete.generator.cspompatterns

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import concrete.CSPOMDriver._
import cspom.CSPOM
import cspom.CSPOM._
import cspom.variable.CSPOMVariable
import cspom.variable.IntVariable
import cspom.compiler.CSPOMCompiler
import concrete.ParameterManager
import concrete.Solver

class AddToEqTest extends FlatSpec with Matchers {

  "AddToEq" should "simplify adjacency constraints correctly" in {
    val problem = CSPOM { implicit problem =>
      val v19 = IntVariable(1 to 5) as "V19"
      val v6 = IntVariable(1 to 5) as "V6"
      // C17
      ctr(v19 === v6 + 1 | v19 === v6 - 1)
      
      // C14
      val v2 = IntVariable(1 to 5) as "V2"
      ctr(v6 === v2)
    }
    
    val pm = new ParameterManager

    CSPOMCompiler.compile(problem, ConcretePatterns(pm))
    
    println(problem)
    
    //val s = Solver(problem, pm)
    //println(s.next)
  }

}