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
import com.typesafe.scalalogging.slf4j.LazyLogging
import concrete.ParameterManager
import concrete.Solver
import concrete.filter.DC20
import concrete.MAC
import org.junit.Before
import org.junit.After
import concrete.generator.cspompatterns.ConcretePatterns
import org.scalatest.FlatSpec

final class DC20SolvingTest extends FlatSpec with SolvingBehaviors {

  val pm = new ParameterManager
  pm("preprocessor") = classOf[DC20]

  "Solving with DC20" should behave like test()

}
