package concrete.longTest;

import scala.annotation.elidable
import org.junit.Assert._
import org.junit.Test
import concrete.util.Loggable
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

//import SolvingTest._

final class SolvingTest {
  //Solver.loggerLevel = "FINE"
  //ParameterManager("ac3c.queue") = classOf[BinaryHeap[Constraint]]
  import SolvingTest._
  val dlog = Solver.loggerLevel

  @Before
  def before() {
    //Solver.loggerLevel = "INFO"
    ParameterManager("preprocessor") = null
    ParameterManager("closeRelations") = false
  }

  @After
  def after() {
    Solver.loggerLevel = dlog
    ParameterManager("preprocessor") = null
  }

  @Test //(timeout = 40000)
  def crosswordm1() {

    assertTrue(solve("crossword-m1-debug-05-01.xml"));
    assertEquals(48, count("crossword-m1-debug-05-01.xml"));

  }

  @Test //(timeout = 5000)
  def crosswordm2() {
    assertTrue(solve("crossword-m2-debug-05-01.xml"));
    assertEquals(48, count("crossword-m2-debug-05-01.xml"));

  }

  @Test //(timeout = 7000)
  def queens8() {
    assertTrue(solve("queens-8.xml"));
    assertEquals(92, count("queens-8.xml"));

  }

  @Test //(timeout = 7000)
  def queens8AllDiff() {
    assertTrue(solve("queens-8.xml"));
    assertEquals(92, count("queensAllDiff-8.xml.bz2"));

  }

  @Test //(timeout = 30000)
  def queens12_ext() {
    assertTrue(solve("queens-12_ext.xml"));
    assertEquals(14200, count("queens-12_ext.xml"));

  }

  @Test //(timeout = 1000)
  def langford() {
    assertTrue(solve("langford-2-4-ext.xml"));
    assertEquals(2, count("langford-2-4-ext.xml"));

  }

  @Test //(timeout = 1000)
  def zebra() {

    assertTrue(solve("zebra.xml"));
    assertEquals(1, count("zebra.xml"));

  }

  //  @Test //(timeout = 1000)
  //  def dimacs() {
  //    assertTrue(solve("flat30-1.cnf"));
  //    // assertNotNull(solve("clauses-2.cnf.bz2"));
  //    //assertEquals(1, count("flat30-1.cnf"));
  //
  //  }

  @Test //(timeout = 10000)
  def bqwh() {
    assertTrue(solve("bqwh-15-106-0_ext.xml"));
    assertEquals(182, count("bqwh-15-106-0_ext.xml"));
  }

  @Test //(timeout = 10000)
  def bqwhGlb() {
    assertTrue(solve("bqwh-18-141-47_glb.xml.bz2"));
    assertEquals(10, count("bqwh-18-141-47_glb.xml.bz2"));
    //assertEquals(182, count("bqwh-15-106-0_ext.xml"));
  }

  @Test //(timeout = 40000)
  def frb35_17_1() {
    assertTrue(solve("frb35-17-1_ext.xml.bz2"));
    assertEquals(2, count("frb35-17-1_ext.xml.bz2"));
  }

  @Test //(timeout = 7000)
  def scen11_f12() {
    assertFalse(solve("scen11-f12.xml.bz2"));
  }

  @Test //(timeout = 7000)
  def scen11() {
    assertTrue(solve("scen11.xml.bz2"));
  }

  @Test //(timeout = 7000)
  def series() {
    assertTrue(solve("series-15.xml.bz2"));
  }
  //  @Test
  //  def fapp01_0200_0() {
  //    assertNull(solve("fapp01-0200-0.xml"));
  //    assertEquals(0, count("fapp01-0200-0.xml"));
  //
  //  }

  @Test
  def jobshop() {
    assertTrue(solve("e0ddr1-10-by-5-8.xml.bz2"))
  }

  @Test //(timeout = 1000)
  def queens12() {
    assertTrue(solve("queens-12.xml"));
    assertEquals(14200, count("queens-12.xml"));

  }

  @Test
  def tsp() {
    assertTrue(solve("tsp-20-1_ext.xml.bz2"));
    //    assertEquals(14200, count("queens-12.xml"));

  }

  @Test
  def bigleq() {
    assertTrue(solve("bigleq-50.xml"))
    assertEquals(1, count("bigleq-50.xml"))
  }

  @Test
  def queensFZN() {
    assertTrue(solve("test.fzn", false))
  }

  @Test
  def rubiksCubeFZN() {
    assertTrue(solve("1d_rubiks_cube.fzn", false))
  }

}

object SolvingTest extends Loggable {

  def solve(name: String, test: Boolean = true): Boolean = {
    val url = getClass.getResource(name)
    val (cspomProblem, data) = CSPOM.load(url);

    val solver = Solver(cspomProblem);

    println(solver.problem.toString)

    logger.info(solver.problem.toString)

    solver.toIterable.headOption.map { sol =>
      println(sol.toSeq.sortBy((_._1)))

      if (test) {
        val failed = XCSPConcrete.control(sol, data('variables).asInstanceOf[Seq[String]], url)
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
          val failed = XCSPConcrete.control(sol, data('variables).asInstanceOf[Seq[String]], url)
          for (f <- failed) {
            fail(1 + count + "th solution: " + f)
          }

          true
        }
        count + 1
    }
  }
}
