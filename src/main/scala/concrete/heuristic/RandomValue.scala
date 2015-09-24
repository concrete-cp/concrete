package concrete.heuristic;

import concrete.Variable
import concrete.Problem
import scala.util.Random
import concrete.Domain
import concrete.ParameterManager

final class RandomValue(pm: ParameterManager) extends ValueHeuristic {

  def score(variable: Variable, domain: Domain, index: Int) = -index

  override def toString = "random";

  def compute(p: Problem) {
    // Nothing to compute
  }

  private val rand = new Random(0)

  override def selectIndex(variable: Variable, dom: Domain) = {
    dom(rand.nextInt(dom.size))
  }
  def shouldRestart = true
}
