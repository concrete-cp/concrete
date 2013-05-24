package concrete.filter;

import scala.collection.Iterable
import concrete.StatisticsManager
import concrete.constraint.Constraint
import concrete.Problem
import concrete.Variable
import concrete.Statistic
import concrete.util.Loggable
import scala.annotation.tailrec

trait SingletonConsistency extends Filter with Loggable {

  def subFilter: Filter

  @Statistic
  var nbSingletonTests = 0;

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

  @tailrec
  private def process(
    stream: Stream[Variable] = Stream.continually(problem.variables).flatten,
    mark: Option[Variable] = None): Boolean = {

    val variable #:: remaining = stream
    if (mark == Some(variable)) {
      true
    } else {
      logger.info(variable.toString)

      if (singletonTest(variable)) {
        subFilter.reduceAfter(variable) && process(remaining, Some(variable))
      } else {
        process(remaining, mark.orElse(Some(variable)))
      }
    }

  }

  def reduce(): Boolean = subFilter.reduceAll() && process()

  def reduceAfter(constraints: Iterable[Constraint]) =
    throw new UnsupportedOperationException()

  def reduceAll() = reduce()

  /**
   * @return true iff the index has been removed from the domain of the variable
   */
  def check(variable: Variable, index: Int) = {
    // if (logger.isLoggable(Level.FINER)) {
    logger.fine(variable + " <- " + variable.dom.value(index) + "(" + index + ")");
    // }

    problem.push();

    variable.dom.setSingle(index);
    nbSingletonTests += 1;
    val consistent = subFilter.reduceAfter(variable);

    problem.pop();

    if (consistent) {
      false
    } else {
      logger.fine("Removing " + variable + ", " + index);

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