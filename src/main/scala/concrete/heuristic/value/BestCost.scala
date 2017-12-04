package concrete.heuristic.value

import java.util.concurrent.TimeoutException

import com.typesafe.scalalogging.LazyLogging
import concrete._
import concrete.filter.Filter
import concrete.heuristic.{DeadEnd, Decision}

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

class BestCost(pm: ParameterManager, rand: Random) extends BranchHeuristic with LazyLogging {
  private val boundsOnly = pm.getOrElse("bestcost.boundsOnly", 100)
  private var objective: Goal = _
  private var filter: Filter = _

  override def toString: String = "best-cost"

  def branch(variable: Variable, domain: Domain, ps: ProblemState): (Decision, Decision) = {
    var bestScore = Int.MaxValue
    val bestValues = new ArrayBuffer[Int]()


    def checkValue(v: Int) = {
      if (Thread.interrupted()) throw new TimeoutException("Timeout while computing best cost")
      val test = ps.assign(variable, v)

      filter.reduceAfter(Seq((variable, Assignment)), test) match {
        case _: Contradiction => false

        case filteredState: ProblemState =>
          val s = score(filteredState)

          logger.info(s"score of $variable <- $v: $s")
          if (s <= bestScore) {
            if (s < bestScore) {
              bestValues.clear()
              bestScore = s
            }

            bestValues += v
          }
          true
      }
    }

    val newDomain = if (domain.size >= boundsOnly) {
      domain.filterBounds(checkValue)
    } else {
      domain.filter(checkValue)
    }

    if (domain ne newDomain) {
      logger.info(s"Bestcost filtered ${variable.toString(ps)} to $newDomain")
    }

    if (newDomain.isEmpty) {
      (DeadEnd(variable), DeadEnd())
    } else {

      /**
        * TODO: best state must be recomputed, improving this would require all data structures
        * to be fully persistent
        */
      val bestValue = bestValues(rand.nextInt(bestValues.size))
      assignBranch(ps.updateDomNonEmpty(variable, newDomain), variable, bestValue)
    }

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
      case Satisfy => 0
    }
  }

  override def compute(solver: MAC, ps: ProblemState): ProblemState = {
    objective = solver.problem.goal
    filter = solver.filter
    ps
  }

  override def shouldRestart: Boolean = false
}
