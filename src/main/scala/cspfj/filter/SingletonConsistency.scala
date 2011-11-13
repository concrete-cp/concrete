package cspfj.filter;

import scala.collection.Iterable
import cspfj.StatisticsManager
import cspfj.constraint.Constraint
import cspfj.problem.Problem
import cspfj.problem.Variable
import cspfj.util.Statistic
import cspfj.util.Loggable
import scala.annotation.tailrec

trait SingletonConsistency extends Filter with Loggable {

  def subFilter: AC3

  @Statistic
  var nbSingletonTests = 0;

  StatisticsManager.register(classOf[SingletonConsistency])

  def reduceAfter(variable: Variable) = {
    if (variable == null) {
      true;
    } else try {
      reduceAll();
    } catch {
      case _: InterruptedException =>
        throw new IllegalStateException("Filter was unexpectingly interrupted !");
    }
  }

  /**
   * @return true iff the problem has been altered
   */
  @throws(classOf[InterruptedException])
  def singletonTest(variable: Variable): Boolean

  def reduce(): Boolean = {
    if (!subFilter.reduceAll()) {
      false;
    } else {
      val stream = Stream.continually(problem.variables.toStream).flatten

      @tailrec
      def process(variable: Variable, remaining: Stream[Variable], mark: Variable): Boolean = {
        if (mark == variable) {
          true
        } else {
          info(variable.toString)

          if (variable.dom.size > 1 && singletonTest(variable)) {
            if (subFilter.reduceAfter(variable)) {
              process(remaining.head, remaining.tail, variable)
            } else {
              false
            }
          } else {
            process(remaining.head, remaining.tail, variable)
          }
        }

      }
      process(stream.head, stream.tail, null)
    }
  }

  def reduceAfter(constraints: Iterable[Constraint]) =
    throw new UnsupportedOperationException()

  def reduceAll() = reduce()

  /**
   * @return true iff the index has been removed from the domain of the variable
   */
  def check(variable: Variable, index: Int) = {
    // if (logger.isLoggable(Level.FINER)) {
    finer(variable + " <- " + variable.dom.value(index) + "(" + index + ")");
    // }

    problem.push();

    variable.dom.setSingle(index);
    nbSingletonTests += 1;
    val consistent = subFilter.reduceAfter(variable);

    problem.pop();

    if (consistent) {
      false
    } else {
      fine("Removing " + variable + ", " + index);

      variable.dom.remove(index);
      true
    }
  }

  def getStatistics =
    Map("SAC-nbsingletontests" -> nbSingletonTests) ++ subFilter.getStatistics map {
      case (k, v) =>
        "SAC-backend-" + k -> v
    }

}