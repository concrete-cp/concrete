package concrete.heuristic.value

import com.typesafe.scalalogging.LazyLogging
import concrete._
import concrete.filter.Filter
import concrete.heuristic.Branch

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

class BestCost(pm: ParameterManager) extends BranchHeuristic with LazyLogging {
  private val boundsOnly = pm.getOrElse("bestcost.boundsOnly", 100)
  private val rand = new Random(pm.getOrElse("random.seed", 0))
  private var objective: Goal = _
  private var filter: Filter = _

  override def toString: String = "best-cost"

  def branch(variable: Variable, domain: Domain, ps: ProblemState): Branch = {
    var bestScore = Int.MaxValue
    var bestValues = new ArrayBuffer[Int]()


    val candidates = if (domain.size >= boundsOnly) {
      Seq(domain.head, domain.last)
    } else {
      domain.view
    }

    for (v <- candidates) {
      filter.reduceAfter(Seq((variable, Assignment)), ps.assign(variable, v)) match {
        case c: Contradiction =>
          val followingBranch = domain.remove(v)
          return new Branch(
            ps.updateDomNonEmptyNoCheck(variable, followingBranch), Seq((variable, InsideRemoval(domain, followingBranch))),
            c, Seq(),
            s"bestCost found ${variable.toString(ps)} /= $v",
            s"bestCost found ${variable.toString(ps)} /= $v, contradiction"
          )
        case filteredState: ProblemState =>
          val s = objective match {
            case Satisfy => return assignBranch(ps, variable, domain, v)
            case _ => score(filteredState)
          }

          logger.info(s"score of $variable <- $v: $s")
          if (s <= bestScore) {
            if (s < bestScore) {
              bestValues.clear()
              bestScore = s
            }

            bestValues += v
          }

      }
    }

    /**
      * TODO: best state must be recomputed, improving this would require all data structures
      * to be fully persistent
      */
    val bestValue = bestValues(rand.nextInt(bestValues.size))
    assignBranch(ps, variable, domain, bestValue)

    //
    //    val b2 = domain.remove(bestValue)
    //
    //    new Branch(
    //      bestState, Seq(),
    //      ps.updateDomNonEmptyNoCheck(variable, b2), Seq((variable, InsideRemoval(domain, b2))),
    //      s"${variable.toString(ps)} = $bestValue",
    //      s"${variable.toString(ps)} /= $bestValue")
  }


  private def score(ps: ProblemState): Int = {
    objective match {
      case Minimize(obj) => ps.dom(obj).head
      case Maximize(obj) => -ps.dom(obj).last
    }
  }

  override def compute(solver: MAC, ps: ProblemState): ProblemState = {
    objective = solver.problem.goal
    filter = solver.filter
    ps
  }

  override def shouldRestart: Boolean = false
}
