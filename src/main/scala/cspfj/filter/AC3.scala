package cspfj.filter;

import java.util.Queue
import scala.collection.IndexedSeq
import scala.collection.JavaConversions
import cspfj.constraint.Constraint
import cspfj.priorityqueues.BinomialHeap
import cspfj.priorityqueues.Key
import cspfj.problem.Problem
import cspfj.problem.Variable
import scala.annotation.tailrec
import cspfj.priorityqueues.ScalaIOBinomialHeap
import cspfj.util.Loggable
import cspfj.priorityqueues.ScalaNative

/**
 * @author scand1sk
 *
 */
final class AC3(
  val problem: Problem,
  val queue: Queue[Variable]) extends Filter with Loggable {

  // private static final Logger LOGGER = Logger.getLogger(Filter.class
  // .getSimpleName());

  private var revisions = 0;

  private var reviseCount = 0;

  def this(problem: Problem) =
    this(problem, new ScalaIOBinomialHeap[Variable](new Key[Variable]() {
      def getKey(o: Variable) = o.dom.size
    }));

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
  private def setRemovals(v: Variable, constraints: IndexedSeq[Constraint], skip: Constraint, i: Int) {
    if (i >= 0) {
      val c = constraints(i)
      if (c ne skip) {
        c.setRemovals(v.positionInConstraint(i), reviseCount)
      }
      setRemovals(v, constraints, skip, i - 1)
    }
  }

  private def setRemovals(v: Variable, skip: Constraint) {
    setRemovals(v, v.constraints, skip, v.constraints.size - 1)
  }

  @tailrec
  private def updateQueue(prev: Array[Int], constraint: Constraint, scope: Array[Variable], i: Int) {
    if (i >= 0) {
      val variable = scope(i)
      if (prev(i) != variable.dom.size) {
        queue.offer(variable)
        setRemovals(variable, constraint)
      }
      updateQueue(prev, constraint, scope, i - 1)
    }
  }

  def updateQueue(prev: Array[Int], constraint: Constraint) {
    /** Requires high optimization */
    updateQueue(prev, constraint, constraint.scope, constraint.arity - 1)
  }

  @tailrec
  private def prepareQueue(modifiedConstraints: Iterator[Constraint]): Boolean = {
    if (modifiedConstraints.hasNext) {
      val c = modifiedConstraints.next
      c.fillRemovals(reviseCount);

      val prev = c.sizes

      if (c.revise(reviseCount)) {
        c.fillRemovals(-1);
        updateQueue(prev, c)
        prepareQueue(modifiedConstraints)
      } else {
        c.weight += 1;
        false;
      }
    } else true

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
      //info(variable.toString)
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
        val prev = c.sizes
        if (c.revise(reviseCount)) {
          c.fillRemovals(-1)
          updateQueue(prev, c)
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

    problem.constraints.foreach { c =>
      val prev = c.sizes
      assert(c.revise(-1))
      assert(prev.sameElements(c.sizes))
    }
    true;
  }

  override def toString =
    "GAC3rm-var-" + queue.getClass.getSimpleName

  def getStatistics = Map("Revisions" -> revisions)

}
