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

  def reduceAll() = {
    reviseCount += 1;
    queue.clear();
    problem.variables.foreach(queue.offer)
    problem.constraints.foreach(_.fillRemovals(reviseCount))

    reduce()

  }

  def reduceAfter(constraints: Iterable[Constraint]): Boolean = {
    reviseCount += 1;
    queue.clear();

    for (c <- constraints) {
      c.fillRemovals(reviseCount);

      if (!c.revise(revisator, reviseCount)) {
        c.weight += 1
        return false;
      }

      c.fillRemovals(-1);

    }

    reduce()
  }

  def reduceFrom(modVar: Array[Int], modCons: Array[Int], cnt: Int): Boolean = {
    reviseCount += 1;
    queue.clear();
    // LOGGER.fine("reduce after " + cnt);
    problem.variables.foreach(v =>
      if (modVar(v.getId) > cnt) queue.offer(v))

    if (modCons != null) {
      // final BitSet cons = new BitSet();
      for (c <- problem.constraints) {
        if (modCons(c.getId) > cnt) {
          // cons.set(c.getId());

          c.fillRemovals(reviseCount);

          if (!c.revise(revisator, reviseCount)) {
            c.weight += 1;
            return false;
          }

          c.fillRemovals(-1);

        }
      }

      // for (int i = modCons.length; --i >= 0;) {
      // assert modCons[i] <= cnt || cons.get(i);
      // }
    }

    reduce();
  }

  def reduceAfter(variable: Variable) = {
    reviseCount += 1;
    if (variable == null) {
      true;
    } else {
      queue.clear();

      queue.offer(variable);

      variable.constraints.zipWithIndex.foreach {
        case (c, i) =>
          c.setRemovals(variable.positionInConstraint(i),
            reviseCount);
      }

      reduce()
    }
  }

  private val revisator = new RevisionHandler() {
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

  @tailrec
  private def reduce(): Boolean = {
    // LOGGER.finer("Reducing");
    if (queue.isEmpty) {
      assert(control)
      true
    } else if (!reduce(queue.poll())) {
      false
    } else {
      reduce()
    }

  }

  private def reduce(variable: Variable): Boolean = {
    variable.constraints.foreach { c =>

      if (!c.isEntailed
        && !c.hasNoRemovals(reviseCount)) {

        revisions += 1;
        if (!c.revise(revisator, reviseCount)) {
          c.weight += 1;
          return false;
        }

        c.fillRemovals(-1);
      }
    }
    return true;
  }

  private def control() = {
    // LOGGER.fine("Control");
    val controlRevisator = new RevisionHandler() {

      def revised(constraint: Constraint,
        variable: Variable) {
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
