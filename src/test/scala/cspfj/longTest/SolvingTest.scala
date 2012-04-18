package cspfj.longTest;

import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Test
import cspfj.util.Loggable
import cspfj.Solver
import cspfj.SolverIterator
import cspom.compiler.ProblemCompiler
import cspom.CSPOM
import cspfj.ParameterManager
import cspfj.priorityqueues.Fifos
import cspfj.constraint.Constraint
import cspfj.priorityqueues.BitVectorPriorityQueue
import cspfj.priorityqueues.JavaFifos
import cspfj.priorityqueues.JavaSimpleFifos
import cspfj.priorityqueues.BinaryHeap

final class SolvingTest extends Loggable {
  //Solver.loggerLevel = "FINE"
  ParameterManager("ac3c.queue") = classOf[BinaryHeap[Constraint]]
  ParameterManager("preprocessor") = null

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
    assertTrue(solve("flat30-1.cnf").isDefined);
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
    assertEquals(solve("scen11-f12.xml.bz2"), None);
  }
  @Test //(timeout = 7000)
  def scen11() {
    assertTrue(solve("scen11.xml.bz2").isDefined);
  }

  @Test //(timeout = 7000)
  def series() {
    assertTrue(solve("series-15.xml.bz2").isDefined);
  }
  //  @Test
  //  def fapp01_0200_0() {
  //    assertNull(solve("fapp01-0200-0.xml"));
  //    assertEquals(0, count("fapp01-0200-0.xml"));
  //
  //  }

  @Test //(timeout = 1000)
  def queens12() {
    assertTrue(solve("queens-12.xml").isDefined);
    //assertEquals(14200, count("queens-12.xml"));

  }

  @Test
  def tsp() {
    assertTrue(solve("tsp-20-1_ext.xml.bz2").isDefined);
    //    assertEquals(14200, count("queens-12.xml"));

  }

  @Test
  def queens4() {
    //assertTrue(solve("queens-12.xml").isDefined);
    assertEquals(2, count("queens-4.xml"));

  }

  @Test
  def bigleq() {
    assertEquals(1, count("bigleq-50.xml"))
  }

  private def solve(name: String) = {
    val cspomProblem = CSPOM.load(getClass.getResource(name));
    ProblemCompiler.compile(cspomProblem);

    val solver = Solver.factory(cspomProblem);

    val sol = new SolverIterator(solver).toStream.headOption

    if (sol.isDefined) {
      val failed = cspomProblem.controlInt(sol.get);
      assertTrue(sol.get + "\n" + failed.toString, failed.isEmpty)
    }

    sol
  }

  private def count(name: String) = {
    val cspomProblem = CSPOM.load(getClass.getResource(name));
    ProblemCompiler.compile(cspomProblem);

    // System.out.println(problem);

    val solver = Solver.factory(cspomProblem);
    var count = 0
    for (solution <- new SolverIterator(solver)) {
      count += 1
      logger.info(solution.toString)
      val failed = cspomProblem.controlInt(solution)
      assertTrue(1 + count + "th solution: " + failed.toString(), failed.isEmpty);
    }

    count;
  }
}
