package concrete.longTest;

import org.scalatest.FlatSpec
import org.scalatest.Inspectors
import org.scalatest.Matchers
import com.typesafe.scalalogging.LazyLogging
import concrete.ParameterManager
import concrete.SlowTest
import concrete.SolverFactory
import concrete.heuristic.revision.DomCtr
import concrete.runner.XCSPConcrete
import cspom.CSPOM
import scala.collection.mutable.LinkedHashMap

//import SolvingTest._

class SolvingTest extends FlatSpec with SolvingBehaviors {

  behavior of "default parameters"

  it should behave like test()
}

trait SolvingBehaviors extends Matchers with Inspectors with LazyLogging { this: FlatSpec =>

  val problemBank = LinkedHashMap[String, AnyVal](
    "bqwh-18-141-47_glb.xml.bz2" -> 10,
    "crossword-m1-debug-05-01.xml" -> 48,
    "crossword-m2-debug-05-01.xml" -> 48,
    "queens-8.xml" -> 92,
    "zebra.xml" -> 1,
    "scen11-f12.xml.bz2" -> 0,
    "scen11.xml.bz2" -> true,
    "bqwh-15-106-0_ext.xml" -> 182,
    "queensAllDiff-8.xml.bz2" -> 92,
    "queens-12_ext.xml" -> 14200,
    "langford-2-4-ext.xml" -> 2,
    "flat30-1.cnf" -> true,
    "frb35-17-1_ext.xml.bz2" -> 2,
    "series-15.xml.bz2" -> true,
    "e0ddr1-10-by-5-8.xml.bz2" -> true,
    "queens-12.xml" -> 14200,
    "tsp-20-1_ext.xml.bz2" -> true,
    "bigleq-50.xml" -> 1,
    "test.fzn" -> true,
    "1d_rubiks_cube.fzn" -> true)

  def test(parameters: ParameterManager = new ParameterManager()): Unit = {
    for ((p, r) <- problemBank) {
      val test = p.contains(".xml")

      val expected: Boolean = r match {
        case e: Int => e > 0
        case b: Boolean => b
        case _ => throw new IllegalArgumentException
      }

      it should "solve " + p taggedAs (SlowTest) in {
        solve(p, expected, parameters, test)
      }
      r match {
        case e: Int if e > 0 => it should s"find $e solutions to $p" taggedAs (SlowTest) in {
          count(p, e, parameters, false)
        }
        case _ =>
      }

    }
  }

  def solve(name: String, expectedResult: Boolean, parameters: ParameterManager, test: Boolean = true): Unit = {

    val url = getClass.getResource(name)

    require(url != null, "Could not find resource " + name)

    val (cspomProblem, data) = CSPOM.load(url);

    val solver = new SolverFactory(parameters)(cspomProblem)

    //    println(solver.concreteProblem)

    val solvIt = solver.toIterable

    if (expectedResult) {
      solvIt should not be 'empty
    } else {
      solvIt shouldBe 'empty
    }

    // println(solver.concreteProblem.toString)

    if (test) {
      for (sol <- solver.toIterable.headOption) {
        val failed = XCSPConcrete.controlCSPOM(sol, data('variables).asInstanceOf[Seq[String]], url)
        withClue(sol) {
          failed shouldBe 'empty
        }
      }
    }

  }

  def count(name: String, expectedResult: Int, parameters: ParameterManager = new ParameterManager(),
    test: Boolean): Unit = {
    val url = getClass.getResource(name)

    require(url != null, "Could not find resource " + name)

    val (cspomProblem, data) = CSPOM.load(url);

    val solver = new SolverFactory(parameters)(cspomProblem)

    val sols = solver.toStream.take(expectedResult + 1)

    if (test) {
      forAll(sols) { sol =>
        val failed = XCSPConcrete.controlCSPOM(sol, data('variables).asInstanceOf[Seq[String]], url)
        failed shouldBe 'empty
      }
    }

    sols should have size expectedResult
  }
}
