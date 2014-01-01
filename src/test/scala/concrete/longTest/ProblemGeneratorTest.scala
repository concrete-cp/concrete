package concrete.longTest;

import java.util.logging.Level.WARNING
import org.junit.Test
import concrete.filter.ACC
import concrete.generator.ProblemGenerator
import concrete.util.Loggable
import cspom.compiler.ProblemCompiler
import cspom.CSPOM
import concrete.Solver
import concrete.generator.cspompatterns.Patterns

final class ProblemGeneratorTest extends Loggable {
  setLevel(WARNING)

  @Test
  def zebra() {
    generateTest("zebra.xml");
  }

  @Test
  def queens12() {
    generateTest("queens-12.xml");
  }

  @Test
  def scen11f12() {
    generateTest("scen11-f12.xml.bz2");
  }

  @Test
  def crosswordm2() {
    generateTest("crossword-m2-debug-05-01.xml");
  }

  @Test
  def lexHerald() {
    generateTest("normalized-crossword-m1-lex-15-04.xml.bz2");
  }

  // @Test
  // public void fapp01_0200_0() throws CSPParseException, IOException,
  // FailedGenerationException, ClassNotFoundException {
  // generateTest("fapp01-0200-0.xml");
  // }

  private def generateTest(file: String) {
    val cspom = CSPOM.load(classOf[ProblemGeneratorTest].getResource(file))._1;

    logger.info(cspom + "\n" + cspom.variables.size + " vars, " + cspom.constraints.size + " cons")

    ProblemCompiler.compile(cspom, Patterns());

    logger.info(cspom + "\n" + cspom.variables.size + " vars, " + cspom.constraints.size + " cons")

    val problem = ProblemGenerator.generate(cspom);

    logger.info(problem + "\n" + problem.variables.size + " vars, " + problem.constraints.size + " cons")

    new ACC(problem).reduceAll();

    logger.info(problem.toString);
  }
}
