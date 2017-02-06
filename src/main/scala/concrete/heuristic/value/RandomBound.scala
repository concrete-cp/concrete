package concrete
package heuristic
package value;

import scala.util.Random

final class RandomBound(pm: ParameterManager) extends ValueHeuristic {

  def score(variable: Variable, domain: Domain, index: Int) = ???

  override def toString = "random-bound";

  def compute(p: Problem) {
    // Nothing to compute
  }

  private val rand = {
    val seed = pm.getOrElse("heuristic.value.seed", 0L) + pm.getOrElse("iteration", 0)
    new Random(seed)
  }

  override def selectIndex(variable: Variable, dom: Domain) = {
    if (rand.nextBoolean()) {
      dom.head
    } else {
      dom.last
    }
  }
  def shouldRestart = true
}
