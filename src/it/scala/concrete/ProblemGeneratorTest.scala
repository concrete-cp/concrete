package concrete

import com.typesafe.scalalogging.LazyLogging
import concrete.filter.ACC
import concrete.generator.ProblemGenerator
import concrete.generator.cspompatterns.ConcretePatterns
import concrete.runner.XCSP3Concrete
import cspom.compiler.CSPOMCompiler

final class ProblemGeneratorTest extends FlatSpec with LazyLogging with TryValues {

  "ProblemGenerator" should "generate zebra" in {
    generateTest("Zebra.xml.xz");
  }

  it should "generate queens-12" in {
    generateTest("queens-12.xml.xz");
  }

  //  it should "generate scen11-f12" in {
  //    generateTest("scen11-f12.xml.bz2");
  //  }

  it should "generate crosswordm2" in {
    generateTest("crossword-m2-debug-05-01.xml.xz");
  }

  it should "generate lexHerald" in {
    generateTest("normalized-crossword-m1-lex-15-04.xml.xz");
  }

  // @Test
  // public void fapp01_0200_0() throws CSPParseException, IOException,
  // FailedGenerationException, ClassNotFoundException {
  // generateTest("fapp01-0200-0.xml");
  // }

  private def generateTest(file: String): Unit = {

    val pm = new ParameterManager()
    val cspom = XCSP3Concrete
      .loadCSPOMURL(classOf[ProblemGeneratorTest].getResource(file))
      .get

    logger.info(cspom + "\n" + cspom.referencedExpressions.size + " vars, " + cspom.constraints.size + " cons")

    CSPOMCompiler.compile(cspom, ConcretePatterns(pm)).get

    logger.info(cspom + "\n" + cspom.referencedExpressions.size + " vars, " + cspom.constraints.size + " cons")

    val problem = new ProblemGenerator(pm).generate(cspom).get._1
    //    match {
    //      case Success((problem, _)) => problem
    //      case Failure(e)            => fail(e)
    //    }

    logger.info(problem + "\n" + problem.variables.size + " vars, " + problem.constraints.size + " cons")

    new ACC(problem, pm).reduceAll(problem.initState.toState) match {
      case _: Contradiction => logger.info("UNSAT")
      case newState: ProblemState => logger.info(problem.toString(newState));
    }

  }
}
