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
import concrete.filter.SAC1
import concrete.MAC
import org.junit.After
import org.junit.Before
import concrete.generator.cspompatterns.ConcretePatterns
import org.scalatest.Assertions
import org.scalatest.FlatSpec

final class SACSolvingTest extends FlatSpec with SolvingBehaviors {

  val pm = new ParameterManager
  
  pm("preprocessor") = classOf[SAC1]

  "Solver with SAC" should behave like test(pm)

}
