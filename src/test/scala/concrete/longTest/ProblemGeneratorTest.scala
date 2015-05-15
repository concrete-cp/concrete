package concrete.longTest;

import org.scalatest.FlatSpec
import com.typesafe.scalalogging.LazyLogging
import concrete.Contradiction
import concrete.ParameterManager
import concrete.ProblemState
import concrete.filter.ACC
import concrete.generator.FailedGenerationException
import concrete.generator.ProblemGenerator
import concrete.generator.cspompatterns.ConcretePatterns
import cspom.CSPOM
import cspom.compiler.CSPOMCompiler
import org.scalatest.TryValues

final class ProblemGeneratorTest extends FlatSpec with LazyLogging with TryValues {

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
    val cspom = CSPOM.load(classOf[ProblemGeneratorTest].getResource(file)).success.value._1;

    logger.info(cspom + "\n" + cspom.referencedExpressions.size + " vars, " + cspom.constraints.size + " cons")

    CSPOMCompiler.compile(cspom, ConcretePatterns(pm));

    logger.info(cspom + "\n" + cspom.referencedExpressions.size + " vars, " + cspom.constraints.size + " cons")

    val problem = new ProblemGenerator(pm).generate(cspom).success.value._1;

    logger.info(problem + "\n" + problem.variables.size + " vars, " + problem.constraints.size + " cons")

    new ACC(problem, pm).reduceAll(problem.initState.toState) match {
      case Contradiction          => logger.info("UNSAT")
      case newState: ProblemState => logger.info(problem.toString(newState));
    }

  }
}
