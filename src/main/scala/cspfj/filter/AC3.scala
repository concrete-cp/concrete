package cspfj.filter;

import java.util.Queue
import scala.annotation.tailrec
import cspfj.constraint.Constraint
import cspfj.constraint.Removals
import cspfj.priorityqueues.Key
import cspfj.priorityqueues.ScalaFifos
import cspfj.priorityqueues.ScalaIOBinomialHeap
import cspfj.priorityqueues.ScalaNative
import cspfj.problem.Problem
import cspfj.problem.Variable
import cspfj.util.Loggable
import cspfj.Parameter
import cspfj.ParameterManager
import cspfj.Statistic

/**
 * @author scand1sk
 *
 */
object AC3 {
  @Parameter("ac3v.queue")
  var queueType: Class[_ <: Queue[Variable]] = classOf[ScalaFifos[Variable]]

  @Parameter("ac3v.key")
  val keyType: Class[_ <: Key[Variable]] = classOf[cspfj.heuristic.revision.Dom]

  def key = keyType.getConstructor().newInstance()

  def queue = queueType.getConstructor(classOf[Key[Variable]]).newInstance(key)

  ParameterManager.register(this);
}

final class AC3(
  val problem: Problem,
  val queue: Queue[Variable]) extends Filter with Loggable {

  // private static final Logger LOGGER = Logger.getLogger(Filter.class
  // .getSimpleName());

  @Statistic
  var revisions = 0;

  def this(problem: Problem) =
    this(problem, AC3.queue)

  def reduceAll() = {
    queue.clear();
    problem.variables.foreach(queue.offer)
    problem.constraints.foreach { c =>
      (0 until c.arity).foreach(c.setRemovals)
    }

    reduce()

  }

  private def prepareQueue(v: Variable) {
    queue.offer(v)
    v.constraints.zipWithIndex.foreach {
      case (c, i) =>
        c.setRemovals(v.positionInConstraint(i))
    }
  }

  private def setRemovals(v: Variable, skip: Constraint) {

    val constraints = v.constraints

    def setR(i: Int) {
      if (i >= 0) {
        val c = constraints(i)
        if (c ne skip)
          c.setRemovals(v.positionInConstraint(i))

        setR(i - 1)
      }
    }

    setR(v.constraints.size - 1)
  }

  def updateQueue(prev: Array[Int], constraint: Constraint) {
    /** Requires high optimization */

    val scope = constraint.scope

    @tailrec
    def uQ(i: Int) {
      if (i >= 0) {
        val variable = scope(i)
        if (prev(i) != variable.dom.size) {
          queue.offer(variable)
          setRemovals(variable, constraint)
        }
        uQ(i - 1)
      }
    }

    uQ(constraint.arity - 1)
  }

  @tailrec
  private def prepareQueue(modifiedConstraints: Iterator[Constraint]): Boolean = {
    if (modifiedConstraints.hasNext) {
      val c = modifiedConstraints.next
      (0 until c.arity).foreach(c.setRemovals)

      val prev = c.sizes

      if (c.consistentRevise()) {

        c.clearRemovals()

        updateQueue(prev, c)
        prepareQueue(modifiedConstraints)
      } else {
        c.weight += 1;
        false;
      }
    } else true

  }

  def reduceAfter(constraints: Iterable[Constraint]): Boolean = {
    Removals.clear()
    queue.clear()
    prepareQueue(constraints.iterator) && reduce()
  }

  def reduceAfter(variable: Variable) =
    variable == null || {
      Removals.clear()
      queue.clear()
      prepareQueue(variable)
      reduce()
    }

  def reduceFrom(modVar: Array[Int], modCons: Array[Int], cnt: Int): Boolean = {
    Removals.clear()
    queue.clear();
    // LOGGER.fine("reduce after " + cnt);

    for (v <- problem.variables if modVar(v.getId) > cnt)
      prepareQueue(v)

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
      assert(AC3Constraint.control(problem))
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
      if (c.isEntailed) {
        reduce(itr)
      } else {
        revisions += 1
        val prev = c.sizes
        //logger.fine("Revising " + c)
        if (c.consistentRevise()) {
          c.clearRemovals()

          updateQueue(prev, c)
          reduce(itr)
        } else {
          c.weight += 1;
          false;
        }
      }
    } else true

  }

  override def toString =
    "GAC3rm-var-" + queue.getClass.getSimpleName

  def getStatistics = Map("Revisions" -> revisions)

}
