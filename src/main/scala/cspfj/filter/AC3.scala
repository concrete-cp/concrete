package cspfj.filter;

import java.util.Queue
import scala.collection.IndexedSeq
import scala.collection.JavaConversions
import cspfj.constraint.Constraint
import cspfj.priorityqueues.BinomialHeap
import cspfj.priorityqueues.Key
import cspfj.problem.Problem
import cspfj.problem.Variable;
import scala.annotation.tailrec

/**
 * @author scand1sk
 *
 */
final class AC3(
  val problem: Problem,
  val queue: Queue[Variable]) extends Filter {

  // private static final Logger LOGGER = Logger.getLogger(Filter.class
  // .getSimpleName());

  private var revisions = 0;

  private var reviseCount = 0;

  def this(problem: Problem) =
    this(problem, new BinomialHeap[Variable](new Key[Variable]() {
      def getKey(o: Variable) = o.dom.size
    }));

  private val rh = new RevisionHandler() {
    def revised(constraint: Constraint, variable: Variable) {
      queue.offer(variable);

      variable.constraints.zipWithIndex.foreach {
        case (c, i) =>
          if (c != constraint) {
            c.setRemovals(variable.positionInConstraint(i), reviseCount)
          }
      }

    }
  };

  def reduceAll() = {
    reviseCount += 1;
    queue.clear();
    problem.variables.foreach(queue.offer)
    problem.constraints.foreach(_.fillRemovals(reviseCount))

    reduce()

  }

  private def prepareQueue(v: Variable) {
    queue.offer(v)
    v.constraints.zipWithIndex.foreach {
      case (c, i) =>
        c.setRemovals(v.positionInConstraint(i), reviseCount)
    }
  }

  @tailrec
  private def prepareQueue(modifiedConstraints: Iterator[Constraint]): Boolean = {
    if (modifiedConstraints.hasNext) {
      val c = modifiedConstraints.next
      c.fillRemovals(reviseCount);

      /** RevisionHandler will add appropriate variables to the queue */
      if (c.revise(rh, reviseCount)) {
        c.fillRemovals(-1);
        prepareQueue(modifiedConstraints)
      } else {
        c.weight += 1;
        false;
      }
    } else {
      true
    }
  }

  def reduceAfter(constraints: Iterable[Constraint]): Boolean = {
    reviseCount += 1
    queue.clear()
    prepareQueue(constraints.iterator) && reduce()
  }

  def reduceAfter(variable: Variable) =
    variable == null || {
      reviseCount += 1
      queue.clear()
      prepareQueue(variable)
      reduce()
    }

  def reduceFrom(modVar: Array[Int], modCons: Array[Int], cnt: Int): Boolean = {
    reviseCount += 1;
    queue.clear();
    // LOGGER.fine("reduce after " + cnt);

    problem.variables.iterator.filter(v => modVar(v.getId) > cnt).foreach(prepareQueue);

    if (modCons == null || prepareQueue(problem.constraints.iterator.filter(c => modCons(c.getId) > cnt))) {
      reduce()
    } else {
      false
    }

  }

  @tailrec
  private def reduce(): Boolean = {
    // LOGGER.finer("Reducing");
    if (queue.isEmpty) {
      assert(control)
      true
    } else {
      val variable = queue.poll()
      if (reduce(variable.constraints.iterator)) {
        reduce()
      } else {
        false
      }
    }

  }

  @tailrec
  private def reduce(itr: Iterator[Constraint]): Boolean = {
    if (itr.hasNext) {
      val c = itr.next
      if (c.isEntailed || c.hasNoRemovals(reviseCount)) {
        reduce(itr)
      } else {
        revisions += 1;
        if (c.revise(rh, reviseCount)) {
          c.fillRemovals(-1)
          reduce(itr)
        } else {
          c.weight += 1;
          false;
        }
      }
    } else true

  }

  private def control() = {
    // LOGGER.fine("Control");
    val controlRevisator = new RevisionHandler() {

      def revised(constraint: Constraint, variable: Variable) {
        assert(false, constraint + ", " + variable)
      }

    };

    problem.constraints.foreach { c =>
      assert(c.revise(controlRevisator, -1))
    }
    true;
  }

  override def toString =
    "GAC3rm-var-" + queue.getClass.getSimpleName

  def getStatistics = Map("Revisions" -> revisions)

}
