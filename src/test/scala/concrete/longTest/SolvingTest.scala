package concrete.longTest;

import scala.annotation.elidable
import org.junit.Assert._
import org.junit.Test
import com.typesafe.scalalogging.slf4j.LazyLogging
import concrete.ParameterManager
import concrete.Solver
import cspom.compiler.ProblemCompiler
import cspom.CSPOM
import org.junit.After
import org.junit.Before
import concrete.heuristic.revision.DomCtr
import concrete.SAT
import concrete.UNSAT
import concrete.UNKNOWNResult
import concrete.generator.cspompatterns.ConcretePatterns
import concrete.runner.XCSPConcrete
import java.io.File
import org.scalatest.Assertions
import org.scalatest.FlatSpec

//import SolvingTest._

class SolvingTest extends FlatSpec with Assertions {
  //Solver.loggerLevel = "FINE"
  //ParameterManager("ac3c.queue") = classOf[BinaryHeap[Constraint]]
  import SolvingTest._

  "Solving with default parameters" should "solve crossword problems" in {
    assert(solve("crossword-m1-debug-05-01.xml"));
    assertResult(48)(count("crossword-m1-debug-05-01.xml"));

    assert(solve("crossword-m2-debug-05-01.xml"));
    assertResult(48)(count("crossword-m2-debug-05-01.xml"));
  }

  it should "solve queens-8" in {
    assert(solve("queens-8.xml"));
    assertResult(92)(count("queens-8.xml"));
  }

  it should "solve queensAllDiff-8" in {
    assert(solve("queensAllDiff-8.xml.bz2"))

    assertResult(92)(count("queensAllDiff-8.xml.bz2"))

  }

  it should "solve queens-12_ext" in {
    assert(solve("queens-12_ext.xml"));
    assertResult(14200)(count("queens-12_ext.xml"));
  }

  it should "solve langford-2-4-ext" in {
    assert(solve("langford-2-4-ext.xml"));
    assertResult(2)(count("langford-2-4-ext.xml"));
  }

  it should "solve zebra" in {
    assert(solve("zebra.xml"));
    assertResult(1)(count("zebra.xml"));
  }

  it should "solve flat-30-1.cnf" in {
    assert(solve("flat30-1.cnf", false));
  }

  it should "solve bqwh-15-106-0_ext" in {
    assert(solve("bqwh-15-106-0_ext.xml"));
    assertResult(182)(count("bqwh-15-106-0_ext.xml"));
  }

  it should "solve bqwh-18-141-47_glb" in {
    assert(solve("bqwh-18-141-47_glb.xml.bz2"));
    assertResult(10)(count("bqwh-18-141-47_glb.xml.bz2"));
  }

  it should "solve frb35-17-1" in {
    assert(solve("frb35-17-1_ext.xml.bz2"));
    assertResult(2)(count("frb35-17-1_ext.xml.bz2"));
  }

  it should "solve scen11-f12" in {
    assertFalse(solve("scen11-f12.xml.bz2"));
  }

  it should "solve scen11" in {
    assert(solve("scen11.xml.bz2"));
  }

  it should "solve series-15" in {
    assert(solve("series-15.xml.bz2"));
  }

  it should "solve e0ddr1-10-by-5-8" in {
    assert(solve("e0ddr1-10-by-5-8.xml.bz2"))
  }

  it should "solve queens-12" in {
    assert(solve("queens-12.xml"));
    assertResult(14200)(count("queens-12.xml"));
  }

  it should "solve tsp-20-1" in {
    assert(solve("tsp-20-1_ext.xml.bz2"));
  }

  it should "solve bigleq-50" in {
    assert(solve("bigleq-50.xml"))
    assertResult(1)(count("bigleq-50.xml"))
  }

  it should "solve test.fzn" in {
    assert(solve("test.fzn", false))
  }

  it should "solve 1d_rubiks_cube.fzn" in {
    assert(solve("1d_rubiks_cube.fzn", false))
  }
}

object SolvingTest extends LazyLogging {

  def solve(name: String, test: Boolean = true): Boolean = {
    val url = getClass.getResource(name)

    require(url != null, "Could not find resource " + name)

    val (cspomProblem, data) = CSPOM.load(url);
    // println(cspomProblem)
    val solver = Solver(cspomProblem)

    // println(solver.concreteProblem.toString)

    solver.toIterable.headOption.map { sol =>
      if (test) {
        val failed = XCSPConcrete.controlCSPOM(sol, data('variables).asInstanceOf[Seq[String]], url)
        for (f <- failed) {
          fail(sol + "\n" + f)
        }
      }

    } isDefined

  }

  def count(name: String) = {
    val url = getClass.getResource(name)
    val (cspomProblem, data) = CSPOM.load(url);

    val solver = Solver(cspomProblem)

    solver.foldLeft(0) {
      (count, sol) =>
        //logger.info(solution.toString)
        assert {
          val failed = XCSPConcrete.controlCSPOM(sol, data('variables).asInstanceOf[Seq[String]], url)
          for (f <- failed) {
            fail(1 + count + "th solution: " + f)
          }

          true
        }
        count + 1
    }
  }
}
