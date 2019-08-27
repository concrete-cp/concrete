package concrete.generator.cspompatterns

import concrete.CSPOMDriver._
import concrete.{ParameterManager, Solver}
import cspom.CSPOM.{ctr, goal, _}
import cspom.CSPOMGoal.Minimize
import cspom.compiler.CSPOMCompiler
import cspom.variable.{BoolVariable, CSPOMSeq, IntVariable}
import cspom.{CSPOM, UNSATException}
import org.scalatest.{FlatSpec, Matchers, TryValues}

class ACCSETest extends FlatSpec with Matchers with TryValues {

  "ACCSE" should "greatly improve bad GR model" in {
    val ticks = 3
    val max = ticks * ticks

    val problem = CSPOM { implicit problem =>
      val variables = for (i <- 1 to ticks) yield IntVariable(1 to max) as s"T$i"

      for (Seq(xi, xj) <- variables.sliding(2)) {
        ctr(xi < xj)
      }

      for (
        xi <- variables; xj <- variables if xi != xj;
        xk <- variables; xl <- variables if xk != xl &&
          (xi != xk || xj != xl)
      ) {
        ctr(xi - xj !== xk - xl)
      }

      goal(Minimize(variables.last))
    }


    val cspom = CSPOMCompiler.compile(problem, ConcretePatterns(new ParameterManager)).get

    // println(problem)

    val nbDiffs = ticks * (ticks - 1)
    val nbConstraints =
      (ticks - 1) + //lt constraints
        nbDiffs + // diff definitions
        1 // alldiff (constraints are posted on both sides)

    withClue(cspom.toString) {
      cspom.constraints.toSeq should have size nbConstraints
    }
  }

  it should "allow CSPOM to detect contradiction" in {
    val problem = CSPOM { implicit problem =>
      val x = IntVariable.free() as "X"
      val y = IntVariable.free() as "Y"
      val z = IntVariable.free() as "Z"

      ctr(sum(x, y, z) <= 10)
      ctr(sum(x, y, z) >= 11)
    }

    problem.constraints should have size 6

    val solver = Solver(problem)

    solver.failure.exception shouldBe an[UNSATException]
  }

  it should "allow CSPOM to detect sum subexpressions" in {
    val problem = CSPOM { implicit problem =>
      val x = IntVariable(-10 to 10) as "X"
      val y = IntVariable(-10 to 10) as "Y"
      val z = IntVariable(-10 to 10) as "Z"
      val w = IntVariable(-10 to 10) as "W"

      ctr(2 *: x + 3 *: y + z === 5)
      ctr(4 *: x + 6 *: y + w === 5)
    }

    Solver(problem).success

    problem.constraints.toSeq should have size 3
  }

  it should "allow CSPOM to detect bool subexpressions" in {
    val problem = CSPOM { implicit problem =>
      val x = new BoolVariable() as "X"
      val y = new BoolVariable() as "Y"
      val z = new BoolVariable() as "Z"
      val w = new BoolVariable() as "W"

      ctr(clause(x)(y, z))
      ctr(clause(x)(w, y))
    }

    Solver(problem).get

    problem.constraints.toSeq should have size 5
  }

  it should "allow CSPOM to detect min subexpressions" in {
    val problem = CSPOM { implicit problem =>
      val x = IntVariable(-10 to 10) as "X"
      val y = IntVariable(-10 to 10) as "Y"
      val z = IntVariable(-10 to 10) as "Z"
      val w = IntVariable(-10 to 10) as "W"

      ctr(CSPOMSeq(x, y, z).cmin === 1)
      ctr(CSPOMSeq(x, y, w).cmin === 2)

      ctr(CSPOMSeq(x, y, z).cmax === 3)
      ctr(CSPOMSeq(x, y, w).cmax === 4)
    }

    Solver(problem).get

    problem.constraints.toSeq should have size 6
  }


}
