package concrete.longTest;

import java.util.logging.Level.WARNING
import org.junit.Test
import concrete.filter.ACC
import concrete.generator.ProblemGenerator
import com.typesafe.scalalogging.slf4j.LazyLogging
import cspom.compiler.ProblemCompiler
import cspom.CSPOM
import concrete.Solver
import concrete.generator.cspompatterns.ConcretePatterns
import org.scalatest.Assertions
import org.scalatest.FlatSpec
import concrete.ParameterManager

final class ProblemGeneratorTest extends FlatSpec with LazyLogging {

  "ProblemGenerator" should "generate zebra" in {
    generateTest("zebra.xml");
  }

  it should "generate queens-12" in {
    generateTest("queens-12.xml");
  }

  it should "generate scen11-f12" in {
    generateTest("scen11-f12.xml.bz2");
  }

  it should "generate crosswordm2" in {
    generateTest("crossword-m2-debug-05-01.xml");
  }

  it should "generate lexHerald" in {
    generateTest("normalized-crossword-m1-lex-15-04.xml.bz2");
  }

  // @Test
  // public void fapp01_0200_0() throws CSPParseException, IOException,
  // FailedGenerationException, ClassNotFoundException {
  // generateTest("fapp01-0200-0.xml");
  // }

  private def generateTest(file: String) {
    
    val pm = new ParameterManager()
    val cspom = CSPOM.load(classOf[ProblemGeneratorTest].getResource(file))._1;

    
    logger.info(cspom + "\n" + cspom.referencedExpressions.size + " vars, " + cspom.constraints.size + " cons")

    ProblemCompiler.compile(cspom, ConcretePatterns(pm));

    logger.info(cspom + "\n" + cspom.referencedExpressions.size + " vars, " + cspom.constraints.size + " cons")

    val problem = new ProblemGenerator(pm).generate(cspom)._1;

    logger.info(problem + "\n" + problem.variables.size + " vars, " + problem.constraints.size + " cons")

    new ACC(problem, pm).reduceAll();

    logger.info(problem.toString);
  }
}
