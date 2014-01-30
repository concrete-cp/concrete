package concrete.longTest;

import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Test
import concrete.filter.DC1
import concrete.generator.FailedGenerationException
import concrete.generator.ProblemGenerator
import concrete.LearnMethod
import cspom.compiler.ProblemCompiler
import cspom.CSPOM
import concrete.util.Loggable
import concrete.ParameterManager
import concrete.Solver
import concrete.MAC
import org.junit.After
import org.junit.Before
import concrete.generator.cspompatterns.ConcretePatterns

final class DC1SolvingTest extends Loggable {
  //Solver.loggerLevel = "FINER"
  import SolvingTest._

  val dlog = Solver.loggerLevel

  @Before
  def before() {
    //Solver.loggerLevel = "INFO"
    ParameterManager("preprocessor") = classOf[DC1]
    ParameterManager("dc1.addConstraints") = "BIN";
    ParameterManager("closeRelations") = false
  }

  @After
  def after() {
    Solver.loggerLevel = dlog
    ParameterManager("preprocessor") = null
  }

  @Test
  def crosswordm1() {

    assertEquals(48, count("crossword-m1-debug-05-01.xml"));

  }

  @Test
  def crosswordm2() {

    assertEquals(48, count("crossword-m2-debug-05-01.xml"));

  }

  @Test
  def queens8() {

    assertEquals(92, count("queens-8.xml"));

  }

  @Test
  def queens12_ext() {

    assertEquals(14200, count("queens-12_ext.xml"));

  }

  @Test
  def langford() {

    assertEquals(2, count("langford-2-4-ext.xml"));

  }

  @Test
  def zebra() {

    assertEquals(1, count("zebra.xml"));

  }

  @Test
  def dimacs() {
    assertTrue(solve("flat30-1.cnf", false));
    // assertNotNull(solve("clauses-2.cnf.bz2"));
    // assertEquals(1, count("flat30-1.cnf"));

  }

  @Test
  def bqwh() {

    assertEquals(182, count("bqwh-15-106-0_ext.xml"));
  }

  @Test
  def frb35_17_1() {
    // assertNotNull(solve("frb35-17-1_ext.xml.bz2"));
    assertEquals(2, count("frb35-17-1_ext.xml.bz2"));
  }

  //  @Test
  //  def scen11_f12() {
  //    assertEquals(solve("scen11-f12.xml.bz2"), None);
  //  }

  //  @Test
  //  def fapp01_0200_0() {
  //    assertNull(solve("fapp01-0200-0.xml"));
  //    assertEquals(0, count("fapp01-0200-0.xml"));
  //
  //  }

  @Test
  def queens12() {
    assertTrue(solve("queens-12.xml"));
    //    assertEquals(14200, count("queens-12.xml"));

  }

}
