package concrete.heuristic.value

import java.util.concurrent.TimeoutException

import com.typesafe.scalalogging.LazyLogging
import concrete._
import concrete.filter.Filter

import scala.collection.immutable.TreeSet
import scala.util.Random

class BestCost(pm: ParameterManager, rand: Random) extends ValueSelector with LazyLogging {
  private val boundsOnly = pm.getOrElse("bestcost.boundsOnly", 100)
  private var objective: Goal = _
  private var filter: Filter = _

  override def toString: String = "best-cost"

  def select(ps: ProblemState, variable: Variable, candidates: Domain): (Outcome, Domain) = {
    var bestScore = Int.MaxValue
    val bestValues = TreeSet.newBuilder[Int] //new ArrayBuffer[Int]()

    def checkValue(v: Int) = {
      if (Thread.interrupted()) throw new TimeoutException("Timeout while computing best cost")
      val test = ps.assign(variable, v)

      filter.reduceAfter(Seq((variable, Assignment)), test) match {
        case _: Contradiction =>
          logger.info(s"$variable <- $v is not SAC")
          false

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

    val domain = ps.dom(variable)

    val newDomain = if (domain.size >= boundsOnly) {
      domain.filterBounds(v => !candidates(v) || checkValue(v))
    } else {
      domain.filter(v => !candidates(v) || checkValue(v))
    }


    val state = if (newDomain.isEmpty) {
      Contradiction(variable)
    } else {
      Event(domain, newDomain)
        .map { e =>
          logger.info(s"Bestcost filtered ${variable.toString(ps)} to $newDomain")
          ps.updateDom(variable, newDomain)
            .andThen { s =>
              filter.reduceAfter(Seq((variable, e)), s)
            }
        }
        .getOrElse {
          ps
        }
    }

    /**
      * TODO: best state must be recomputed, improving this would require all data structures
      * to be fully persistent
      */
    (state, IntDomain.ofTreeSet(bestValues.result()))

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
