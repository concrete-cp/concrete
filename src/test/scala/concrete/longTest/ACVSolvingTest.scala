package concrete.longTest;

import scala.annotation.elidable
import org.junit.Assert._
import org.junit.Assert.assertTrue
import org.junit.Test
import com.typesafe.scalalogging.slf4j.LazyLogging
import concrete.ParameterManager
import concrete.Solver
import cspom.compiler.ProblemCompiler
import cspom.CSPOM
import org.junit.After
import org.junit.Before
import concrete.filter.ACV
import concrete.MAC
import concrete.generator.cspompatterns.ConcretePatterns
import org.scalatest.FlatSpec

final class ACVSolvingTest extends FlatSpec with SolvingBehaviors {
  
  val pm = new ParameterManager
  pm("mac.filter") = classOf[ACV]
  
  "Solver with ACV" should behave like test(pm)

}
