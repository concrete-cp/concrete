package concrete.heuristic.value

import com.typesafe.scalalogging.LazyLogging
import concrete._
import concrete.filter.Filter
import concrete.heuristic.{DeadEnd, Decision}
import concrete.util.Interval

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

class BestCostDicho(pm: ParameterManager) extends BranchHeuristic with LazyLogging {
  private val boundsOnly = pm.getOrElse("bestcost.boundsOnly", 100)
  private val rand = new Random(pm.getOrElse("random.seed", 0))
  private var objective: Goal = _
  private var filter: Filter = _

  override def toString: String = "best-cost"

  private case class BestScoreItv(itv: Interval, score: Int)

  implicit private val bsio: Ordering[BestScoreItv] = Ordering.by(bsi => (-bsi.score, -bsi.itv.size))

  def test(variable: Variable, dom: Domain, current: Interval, ps: ProblemState): Option[Int] = {
    val test = dom & current

    if (test.isEmpty) {
      None
    } else {
      filter.reduceAfter(Seq((variable, BoundRemoval(test))), ps.updateDomNonEmpty(variable, test)) match {
        case _: Contradiction =>
          logger.debug(s"$current -> Contradiction")
          None
        case filteredState: ProblemState =>
          val s = score(filteredState)
          logger.debug(s"$current -> $s")
          Some(s)
      }
    }
  }

  def branch(variable: Variable, domain: Domain, ps: ProblemState): (Decision, Decision) = {
    var bestScore = Int.MaxValue
    val bestValues = new ArrayBuffer[Int]()
    val queue: mutable.PriorityQueue[BestScoreItv] = new mutable.PriorityQueue()

    queue += BestScoreItv(domain.span, score(ps))

    var dom = domain

    while (queue.headOption.exists(_.score <= bestScore)) {
      val BestScoreItv(currentItv, currentScore) = queue.dequeue()
      if (currentItv.size == 1) {
        if (currentScore < bestScore) {
          bestValues.clear()
          bestScore = currentScore
        }
        bestValues += currentItv.lb

      } else {
        for (current <- split(currentItv)) {
          test(variable, dom, current, ps) match {
            case None => dom = dom.removeItv(current.lb, current.ub)
            case Some(s) => queue += BestScoreItv(current, s)

          }
        }
      }
    }

    if (domain ne dom) {
      logger.info(s"Bestcost filtered ${variable.toString(ps)} to $dom")
    }

    if (dom.isEmpty) {
      (DeadEnd(variable), DeadEnd()) //.contradiction(Contradiction(Seq(variable)))
    } else {

      /**
        * TODO: best state must be recomputed, improving this would require all data structures
        * to be fully persistent
        */
      val bestValue = bestValues(rand.nextInt(bestValues.size))
      assignBranch(ps.updateDomNonEmpty(variable, dom), variable, bestValue)
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

  def split(current: Interval): Seq[Interval] = {
    val split = (current.lb + current.ub) / 2
    Seq(Interval(current.lb, split), Interval(split + 1, current.ub))
  }

  override def compute(solver: MAC, ps: ProblemState): ProblemState = {
    objective = solver.problem.goal
    filter = solver.filter
    ps
  }

  override def shouldRestart: Boolean = false

}