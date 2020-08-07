package concrete.it

import concrete.ParameterManager
import concrete.runner.FZConcrete
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FZFDTest extends AnyFlatSpec with Matchers {
  "FlatZinc solver" should "respect fd" in {
    val url = getClass.getResource("concrete_fd_search_error.fzn")

    FZConcrete.load(new ParameterManager(), Seq(url.getFile)).get

    val firstSolution = FZConcrete.cspomSolver.next()

    firstSolution("x") shouldBe (1 to 5)


  }
}
