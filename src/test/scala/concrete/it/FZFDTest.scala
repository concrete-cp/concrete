package concrete.it

import concrete.ParameterManager
import concrete.runner.FZConcrete
import org.scalatest.{FlatSpec, Matchers}

class FZFDTest extends FlatSpec with Matchers {
  "FlatZinc solver" should "respect fd" in {
    val url = getClass.getResource("concrete_fd_search_error.fzn")

    FZConcrete.load(new ParameterManager(), Seq(url.getFile)).get

    val firstSolution = FZConcrete.cspomSolver.next()

    firstSolution("x") shouldBe (1 to 5)


  }
}
