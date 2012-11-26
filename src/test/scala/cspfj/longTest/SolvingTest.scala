package cspfj.longTest;

import scala.annotation.elidable
import org.junit.Assert._
import org.junit.Test
import cspfj.util.Loggable
import cspfj.ParameterManager
import cspfj.Solver
import cspfj.SolverIterator
import cspom.compiler.ProblemCompiler
import cspom.CSPOM
import org.junit.After
import org.junit.Before
import cspfj.heuristic.revision.DomCtr
import cspfj.SAT
import cspfj.UNSAT
import cspfj.UNKNOWNResult

final class SolvingTest extends Loggable {
  //Solver.loggerLevel = "FINE"
  //ParameterManager("ac3c.queue") = classOf[BinaryHeap[Constraint]]

  val dlog = Solver.loggerLevel

  @Before
  def before() {
    //Solver.loggerLevel = "INFO"
    ParameterManager("preprocessor") = null
    ParameterManager("reduction") = "ArrayTrie"
    //ParameterManager("ac3c.key") = classOf[DomCtr]
  }

  @After
  def after() {
    Solver.loggerLevel = dlog
    ParameterManager("preprocessor") = null
  }

  @Test //(timeout = 40000)
  def crosswordm1() {

    //assertTrue(solve("crossword-m1-debug-05-01.xml").isDefined);
    assertEquals(48, count("crossword-m1-debug-05-01.xml"));

  }

  @Test //(timeout = 5000)
  def crosswordm2() {
    //assertTrue(solve("crossword-m2-debug-05-01.xml").isDefined);
    assertEquals(48, count("crossword-m2-debug-05-01.xml"));

  }

  @Test //(timeout = 7000)
  def queens8() {
    //assertTrue(solve("queens-8.xml").isDefined);
    assertEquals(92, count("queens-8.xml"));

  }

  @Test //(timeout = 7000)
  def queens8AllDiff() {
    //assertTrue(solve("queens-8.xml").isDefined);
    assertEquals(92, count("queensAllDiff-8.xml.bz2"));

  }

  @Test //(timeout = 30000)
  def queens12_ext() {
    //assertTrue(solve("queens-12_ext.xml").isDefined);
    assertEquals(14200, count("queens-12_ext.xml"));

  }

  @Test //(timeout = 1000)
  def langford() {
    //assertTrue(solve("langford-2-4-ext.xml").isDefined);
    assertEquals(2, count("langford-2-4-ext.xml"));

  }

  @Test //(timeout = 1000)
  def zebra() {

    //assertTrue(solve("zebra.xml").isDefined);
    assertEquals(1, count("zebra.xml"));

  }

  @Test //(timeout = 1000)
  def dimacs() {
    assertTrue(solve("flat30-1.cnf"));
    // assertNotNull(solve("clauses-2.cnf.bz2"));
    //assertEquals(1, count("flat30-1.cnf"));

  }

  @Test //(timeout = 10000)
  def bqwh() {
    //assertTrue(solve("bqwh-15-106-0_ext.xml").isDefined);
    assertEquals(182, count("bqwh-15-106-0_ext.xml"));
  }

  @Test //(timeout = 10000)
  def bqwhGlb() {
    assertEquals(10, count("bqwh-18-141-47_glb.xml.bz2"));
    //assertEquals(182, count("bqwh-15-106-0_ext.xml"));
  }

  @Test //(timeout = 40000)
  def frb35_17_1() {
    // assertNotNull(solve("frb35-17-1_ext.xml.bz2"));
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
    //assertEquals(14200, count("queens-12.xml"));

  }

  @Test
  def tsp() {
    assertTrue(solve("tsp-20-1_ext.xml.bz2"));
    //    assertEquals(14200, count("queens-12.xml"));

  }

  @Test
  def bigleq() {
    assertEquals(1, count("bigleq-50.xml"))
  }

  private def solve(name: String): Boolean = {
    val cspomProblem = CSPOM.load(getClass.getResource(name));
    ProblemCompiler.compile(cspomProblem);

    
    val solver = Solver.factory(cspomProblem);

    solver.nextSolution() match {
      case SAT(sol: Map[String, Int]) =>
        val failed = cspomProblem.controlInt(sol);
        assertTrue(sol + "\n" + failed.toString, failed.isEmpty)
        true
      case UNSAT => false
      case UNKNOWNResult => fail(); false
    }
  }

  private def count(name: String) = {
    val cspomProblem = CSPOM.load(getClass.getResource(name));
    println(cspomProblem)
    ProblemCompiler.compile(cspomProblem);

    println(cspomProblem)

    val solver = Solver.factory(cspomProblem);
    var count = 0
    for (solution <- new SolverIterator(solver)) {
      count += 1
      logger.info(solution.toString)
      assert {
        val failed = cspomProblem.controlInt(solution)
        assertTrue(1 + count + "th solution: " + failed.toString(), failed.isEmpty);
        true
      }
    }

    count;
  }
}
