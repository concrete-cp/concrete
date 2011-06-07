package cspfj;

import cspfj.generator.ProblemGenerator
import cspfj.problem.Problem
import cspom.compiler.ProblemCompiler
import cspom.constraint.CSPOMConstraint
import cspom.CSPOM
import java.util.logging.Level
import org.junit.Assert.{ assertTrue, assertNull, assertNotNull, assertEquals }
import org.junit.Test
import scala.collection.JavaConversions
import util.Loggable

final class SolvingTest extends Loggable {
  setLevel(Level.WARNING)

  @Test
  def crosswordm1() {

    assertNotNull(solve("crossword-m1-debug-05-01.xml"));
    assertEquals(48, count("crossword-m1-debug-05-01.xml"));

  }

  @Test
  def crosswordm2() {
    assertNotNull(solve("crossword-m2-debug-05-01.xml"));
    assertEquals(48, count("crossword-m2-debug-05-01.xml"));

  }

  @Test
  def queens8() {
    assertNotNull(solve("queens-8.xml"));
    assertEquals(92, count("queens-8.xml"));

  }

  @Test
  def queens12_ext() {
    assertNotNull(solve("queens-12_ext.xml"));
    assertEquals(14200, count("queens-12_ext.xml"));

  }

  @Test
  def langford() {
    assertNotNull(solve("langford-2-4-ext.xml"));
    assertEquals(2, count("langford-2-4-ext.xml"));

  }

  @Test
  def zebra() {

    assertNotNull(solve("zebra.xml"));
    assertEquals(1, count("zebra.xml"));

  }

  @Test
  def dimacs() {
    assertNotNull(solve("flat30-1.cnf"));
    // assertNotNull(solve("clauses-2.cnf.bz2"));
    // assertEquals(1, count("flat30-1.cnf"));

  }

  @Test
  def bqwh() {
    assertNotNull(solve("bqwh-15-106-0_ext.xml"));
    assertEquals(182, count("bqwh-15-106-0_ext.xml"));
  }

  @Test
  def frb35_17_1() {
    // assertNotNull(solve("frb35-17-1_ext.xml.bz2"));
    assertEquals(2, count("frb35-17-1_ext.xml.bz2"));
  }

  @Test
  def scen11_f12() {
    assertNull(solve("scen11-f12.xml.bz2"));
  }

  @Test
  def fapp01_0200_0() {
    assertNull(solve("fapp01-0200-0.xml"));
    assertEquals(0, count("fapp01-0200-0.xml"));

  }

  @Test
  def queens12() {
    assertNotNull(solve("queens-12.xml"));
    assertEquals(14200, count("queens-12.xml"));

  }

  private def solve(name: String) = {
    val cspomProblem = CSPOM.load(getClass.getResource("problem/" + name));
    ProblemCompiler.compile(cspomProblem);
    val problem = ProblemGenerator.generate(cspomProblem);

    val solver = new MGACIter(problem);

    val solution = JavaConversions.mapAsScalaMap(JavaConversions.iterableAsScalaIterable(
      solver).head);

    if (solution != null) {
      val failed = cspomProblem.controlInteger(solution);
      assertTrue(failed.toString, failed.isEmpty)
    }
    solution;
  }

  private def count(name: String) = {
    val cspomProblem = CSPOM.load(getClass.getResource("problem/" + name));
    ProblemCompiler.compile(cspomProblem);
    val problem = ProblemGenerator.generate(cspomProblem);
    // System.out.println(problem);

    val solver = new MGACIter(problem);
    var count = 0
    for (solution <- JavaConversions.iterableAsScalaIterable(solver)) {
      count += 1
      val failed = cspomProblem.controlInteger(JavaConversions.mapAsScalaMap(solution));
      assertTrue(1 + count + "th solution: " + failed.toString(), failed.isEmpty);
    }

    count;
  }
}
